package org.smartregister.reveal.presenter;

import android.support.v4.util.Pair;
import android.text.TextUtils;
import android.util.Log;

import com.google.gson.reflect.TypeToken;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.domain.Campaign;
import org.smartregister.domain.form.FormLocation;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.util.AssetHandler;

import java.util.ArrayList;
import java.util.List;

import static org.smartregister.reveal.contract.ListTaskContract.ListTaskView;
import static org.smartregister.reveal.util.Constants.Tags.COUNTRY;
import static org.smartregister.reveal.util.Constants.Tags.DISTRICT;
import static org.smartregister.reveal.util.Constants.Tags.HEALTH_CENTER;
import static org.smartregister.reveal.util.Constants.Tags.OPERATIONAL_AREA;
import static org.smartregister.reveal.util.Constants.Tags.PROVINCE;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public class ListTaskPresenter implements ListTaskContract.PresenterCallBack {

    private static final String TAG = "ListTaskPresenter";

    private ListTaskView listTaskView;

    private ListTaskInteractor listTaskInteractor;

    private LocationHelper locationHelper;

    public ListTaskPresenter(ListTaskView listTaskView) {
        this.listTaskView = listTaskView;
        listTaskInteractor = new ListTaskInteractor();
        locationHelper = LocationHelper.getInstance();
    }

    public void onInitializeDrawerLayout() {

        listTaskView.setOperator();

        if (StringUtils.isBlank(PreferencesUtil.getInstance().getCurrentOperationalArea())) {
            ArrayList<String> operationalAreaLevels = new ArrayList<>();
            operationalAreaLevels.add(DISTRICT);
            operationalAreaLevels.add(HEALTH_CENTER);
            List<String> defaultLocation = locationHelper.generateDefaultLocationHierarchy(operationalAreaLevels);

            listTaskView.setDistrict(defaultLocation.get(0));
            listTaskView.setFacility(defaultLocation.get(1));
        } else {
            populateLocationsFromPreferences();
        }

        listTaskView.setCampaign(PreferencesUtil.getInstance().getCurrentCampaign());

    }

    public void onShowOperationalAreaSelector() {
        listTaskView.showOperationalAreaSelector(extractLocationHierarchy());

    }

    private Pair<String, ArrayList<String>> extractLocationHierarchy() {

        ArrayList<String> operationalAreaLevels = new ArrayList<>();
        operationalAreaLevels.add(COUNTRY);
        operationalAreaLevels.add(PROVINCE);
        operationalAreaLevels.add(DISTRICT);
        operationalAreaLevels.add(OPERATIONAL_AREA);

        List<String> defaultLocation = locationHelper.generateDefaultLocationHierarchy(operationalAreaLevels);

        List<FormLocation> entireTree = locationHelper.generateLocationHierarchyTree(false, operationalAreaLevels);


        String entireTreeString = AssetHandler.javaToJsonString(entireTree,
                new TypeToken<List<FormLocation>>() {
                }.getType());

        return new Pair<>(entireTreeString, new ArrayList<>(defaultLocation));
    }

    private void populateLocationsFromPreferences() {
        listTaskView.setDistrict(PreferencesUtil.getInstance().getCurrentDistrict());
        listTaskView.setFacility(PreferencesUtil.getInstance().getCurrentFacility());
        listTaskView.setOperationalArea(PreferencesUtil.getInstance().getCurrentOperationalArea());
    }

    public void onOperationalAreaSelectorClicked(ArrayList<String> name) {

        Log.d(TAG, "Selected Location Hierarchy: " + TextUtils.join(",", name));

        PreferencesUtil.getInstance().setCurrentDistrict(name.get(2));
        PreferencesUtil.getInstance().setCurrentOperationalArea(name.get(3));

        ArrayList<String> operationalAreaLevels = new ArrayList<>();
        operationalAreaLevels.add(DISTRICT);
        operationalAreaLevels.add(HEALTH_CENTER);
        operationalAreaLevels.add(OPERATIONAL_AREA);
        List<FormLocation> entireTree = locationHelper.generateLocationHierarchyTree(false, operationalAreaLevels);
        String facility = getFacilityFromOperationalArea(name.get(2), name.get(3), entireTree);
        PreferencesUtil.getInstance().setCurrentFacility(facility);

        populateLocationsFromPreferences();

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
        listTaskInteractor.fetchCampaigns(this);
    }

    @Override
    public void onCampaignsFetched(List<Campaign> campaigns) {
        List<String> ids = new ArrayList<>();
        List<FormLocation> formLocations = new ArrayList<>();
        for (Campaign campaign : campaigns) {
            ids.add(campaign.getIdentifier());
            FormLocation formLocation = new FormLocation();
            formLocation.name = campaign.getTitle();
            formLocation.key = campaign.getIdentifier();
            formLocation.level = "";
            formLocations.add(formLocation);
        }

        String entireTreeString = AssetHandler.javaToJsonString(formLocations,
                new TypeToken<List<FormLocation>>() {
                }.getType());
        listTaskView.showCampaignSelector(ids, entireTreeString);
    }

    public void onCampaignSelectorClicked(ArrayList<String> value, ArrayList<String> name) {

        Log.d(TAG, "Selected Campaign : " + TextUtils.join(",", name));
        Log.d(TAG, "Selected Campaign Ids: " + TextUtils.join(",", value));

        PreferencesUtil.getInstance().setCurrentCampaign(value.get(0));
        listTaskView.setCampaign(name.get(0));
    }
}
