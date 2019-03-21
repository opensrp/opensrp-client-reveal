package org.smartregister.reveal.presenter;

import android.support.v4.util.Pair;
import android.text.TextUtils;
import android.util.Log;

import com.google.gson.reflect.TypeToken;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.domain.form.FormLocation;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.interactor.BaseDrawerInteractor;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.util.AssetHandler;
import org.smartregister.util.Utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.smartregister.AllConstants.OPERATIONAL_AREAS;
import static org.smartregister.reveal.util.Constants.Tags.COUNTRY;
import static org.smartregister.reveal.util.Constants.Tags.DISTRICT;
import static org.smartregister.reveal.util.Constants.Tags.HEALTH_CENTER;
import static org.smartregister.reveal.util.Constants.Tags.OPERATIONAL_AREA;
import static org.smartregister.reveal.util.Constants.Tags.PROVINCE;

/**
 * Created by samuelgithengi on 3/21/19.
 */
public abstract class BaseDrawerPresenter implements BaseDrawerContract.Presenter {

    private static final String TAG = "BaseDrawerPresenter";

    private BaseDrawerContract.View view;

    private PreferencesUtil prefsUtil;

    private LocationHelper locationHelper;

    protected boolean changedCurrentSelection;

    private BaseDrawerContract.Interactor interactor;

    public BaseDrawerPresenter(BaseDrawerContract.View view) {
        this.view = view;
        this.prefsUtil = PreferencesUtil.getInstance();
        this.locationHelper = LocationHelper.getInstance();
        interactor = new BaseDrawerInteractor(this);
    }

    public void onInitializeDrawerLayout() {

        view.setOperator();

        if (StringUtils.isBlank(prefsUtil.getCurrentOperationalArea())) {
            ArrayList<String> operationalAreaLevels = new ArrayList<>();
            operationalAreaLevels.add(DISTRICT);
            operationalAreaLevels.add(HEALTH_CENTER);
            List<String> defaultLocation = locationHelper.generateDefaultLocationHierarchy(operationalAreaLevels);

            if (defaultLocation != null) {
                view.setDistrict(defaultLocation.get(0));
                view.setFacility(defaultLocation.get(1));
            }
        } else {
            populateLocationsFromPreferences();
        }

        view.setCampaign(prefsUtil.getCurrentCampaign());

    }

    private void populateLocationsFromPreferences() {
        view.setDistrict(prefsUtil.getCurrentDistrict());
        view.setFacility(prefsUtil.getCurrentFacility());
        view.setOperationalArea(prefsUtil.getCurrentOperationalArea());
    }

    public void onShowOperationalAreaSelector() {
        Pair<String, ArrayList<String>> locationHierarchy = extractLocationHierarchy();
        if (locationHierarchy == null) {//try to evict location hierachy in cache
            RevealApplication.getInstance().getContext().anmLocationController().evict();
            locationHierarchy = extractLocationHierarchy();
        }
        if (locationHierarchy != null) {
            view.showOperationalAreaSelector(extractLocationHierarchy());
        } else {
            view.displayNotification(R.string.error_fetching_location_hierarchy_title, R.string.error_fetching_location_hierarchy);
            RevealApplication.getInstance().getContext().userService().forceRemoteLogin();
        }

    }

    private Pair<String, ArrayList<String>> extractLocationHierarchy() {

        ArrayList<String> operationalAreaLevels = new ArrayList<>();
        operationalAreaLevels.add(COUNTRY);
        operationalAreaLevels.add(PROVINCE);
        operationalAreaLevels.add(DISTRICT);
        operationalAreaLevels.add(OPERATIONAL_AREA);

        List<String> defaultLocation = locationHelper.generateDefaultLocationHierarchy(operationalAreaLevels);

        if (defaultLocation != null) {
            List<FormLocation> entireTree = locationHelper.generateLocationHierarchyTree(false, operationalAreaLevels);
            List<String> authorizedOperationalAreas = Arrays.asList(StringUtils.split(prefsUtil.getPreferenceValue(OPERATIONAL_AREAS), ','));
            removeUnauthorizedOperationalAreas(authorizedOperationalAreas, entireTree);

            String entireTreeString = AssetHandler.javaToJsonString(entireTree,
                    new TypeToken<List<FormLocation>>() {
                    }.getType());

            return new Pair<>(entireTreeString, new ArrayList<>(defaultLocation));
        } else {
            return null;
        }
    }


    public void onOperationalAreaSelectorClicked(ArrayList<String> name) {

        Log.d(TAG, "Selected Location Hierarchy: " + TextUtils.join(",", name));
        if (name.size() != 4)//no operational area was selected, dialog was dismissed
            return;
        prefsUtil.setCurrentDistrict(name.get(2));
        prefsUtil.setCurrentOperationalArea(name.get(3));

        ArrayList<String> operationalAreaLevels = new ArrayList<>();
        operationalAreaLevels.add(DISTRICT);
        operationalAreaLevels.add(HEALTH_CENTER);
        operationalAreaLevels.add(OPERATIONAL_AREA);
        List<FormLocation> entireTree = locationHelper.generateLocationHierarchyTree(false, operationalAreaLevels);
        String facility = getFacilityFromOperationalArea(name.get(2), name.get(3), entireTree);
        prefsUtil.setCurrentFacility(facility);
        changedCurrentSelection = true;

        populateLocationsFromPreferences();
        unlockDrawerLayout();

    }


    private void removeUnauthorizedOperationalAreas(List<String> operationalAreas, List<FormLocation> entireTree) {

        for (FormLocation countryLocation : entireTree) {
            for (FormLocation provinceLocation : countryLocation.nodes) {
                for (FormLocation districtLocation : provinceLocation.nodes) {
                    List<FormLocation> toRemove = new ArrayList<>();
                    for (FormLocation operationalAreaLocation : districtLocation.nodes) {
                        if (!operationalAreas.contains(operationalAreaLocation.name))
                            toRemove.add(operationalAreaLocation);
                    }
                    districtLocation.nodes.removeAll(toRemove);
                }
            }
        }
    }

    private String getFacilityFromOperationalArea(String district, String operationalArea, List<FormLocation> entireTree) {
        for (FormLocation districtLocation : entireTree) {
            if (!districtLocation.name.equals(district))
                continue;
            for (FormLocation facilityLocation : districtLocation.nodes) {
                for (FormLocation operationalAreaLocation : facilityLocation.nodes) {
                    if (operationalAreaLocation.name.equals(operationalArea)) {
                        return facilityLocation.name;
                    }
                }
            }
        }
        return null;
    }


    public void onShowCampaignSelector() {
        interactor.fetchCampaigns();
    }


    public void onCampaignSelectorClicked(ArrayList<String> value, ArrayList<String> name) {
        if (Utils.isEmptyCollection(name))
            return;
        Log.d(TAG, "Selected Campaign : " + TextUtils.join(",", name));
        Log.d(TAG, "Selected Campaign Ids: " + TextUtils.join(",", value));

        prefsUtil.setCurrentCampaign(name.get(0));
        prefsUtil.setCurrentCampaignId(value.get(0));
        view.setCampaign(name.get(0));
        changedCurrentSelection = true;
        unlockDrawerLayout();

    }

    public abstract void onDrawerClosed();

    private void unlockDrawerLayout() {
        String campaign = PreferencesUtil.getInstance().getCurrentCampaignId();
        String operationalArea = PreferencesUtil.getInstance().getCurrentOperationalArea();
        if (StringUtils.isNotBlank(campaign) &&
                StringUtils.isNotBlank(operationalArea)) {
            view.unlockNavigationDrawer();
        }
    }

}
