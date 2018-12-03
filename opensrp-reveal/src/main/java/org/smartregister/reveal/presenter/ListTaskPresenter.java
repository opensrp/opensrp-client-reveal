package org.smartregister.reveal.presenter;

import android.support.v4.util.Pair;
import android.text.TextUtils;
import android.util.Log;

import com.google.gson.reflect.TypeToken;
import com.mapbox.geojson.Geometry;
import com.mapbox.mapboxsdk.geometry.LatLng;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONObject;
import org.smartregister.domain.Campaign;
import org.smartregister.domain.form.FormLocation;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.view.ListTasksActivity;
import org.smartregister.util.AssetHandler;
import org.smartregister.util.Utils;

import java.util.ArrayList;
import java.util.List;

import static org.smartregister.reveal.contract.ListTaskContract.ListTaskView;
import static org.smartregister.reveal.util.Constants.GeoJSON.FEATURES;
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

    private PreferencesUtil prefsUtil = PreferencesUtil.getInstance();

    private boolean changedCurrentSelection;

    public ListTaskPresenter(ListTaskView listTaskView) {
        this.listTaskView = listTaskView;
        listTaskInteractor = new ListTaskInteractor(this);
        locationHelper = LocationHelper.getInstance();
    }

    public void onInitializeDrawerLayout() {

        listTaskView.setOperator();

        if (StringUtils.isBlank(prefsUtil.getCurrentOperationalArea())) {
            ArrayList<String> operationalAreaLevels = new ArrayList<>();
            operationalAreaLevels.add(DISTRICT);
            operationalAreaLevels.add(HEALTH_CENTER);
            List<String> defaultLocation = locationHelper.generateDefaultLocationHierarchy(operationalAreaLevels);

            listTaskView.setDistrict(defaultLocation.get(0));
            listTaskView.setFacility(defaultLocation.get(1));
        } else {
            populateLocationsFromPreferences();
        }

        listTaskView.setCampaign(prefsUtil.getCurrentCampaign());

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
        listTaskView.setDistrict(prefsUtil.getCurrentDistrict());
        listTaskView.setFacility(prefsUtil.getCurrentFacility());
        listTaskView.setOperationalArea(prefsUtil.getCurrentOperationalArea());
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
        listTaskInteractor.fetchCampaigns();
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

        prefsUtil.setCurrentCampaign(name.get(0));
        prefsUtil.setCurrentCampaignId(value.get(0));
        listTaskView.setCampaign(name.get(0));
        changedCurrentSelection = true;
        unlockDrawerLayout();
    }

    public void onDrawerClosed() {
        if (changedCurrentSelection) {
            listTaskView.showProgressDialog();
            listTaskInteractor.fetchLocations(prefsUtil.getCurrentCampaignId(), prefsUtil.getCurrentOperationalArea());
        }
    }

    @Override
    public void onStructuresFetched(JSONObject structuresGeoJson, Geometry operationalAreaGeometry) {
        listTaskView.hideProgressDialog();
        changedCurrentSelection = false;
        if (structuresGeoJson.has(FEATURES)) {
            listTaskView.setGeoJsonSource(structuresGeoJson.toString(), operationalAreaGeometry);
        } else
            listTaskView.displayNotification(R.string.fetch_structures_failed_message);
    }

    private void unlockDrawerLayout() {
        String campaign = PreferencesUtil.getInstance().getCurrentCampaignId();
        String operationalArea = PreferencesUtil.getInstance().getCurrentOperationalArea();
        if (StringUtils.isNotBlank(campaign) &&
                StringUtils.isNotBlank(operationalArea)) {
            listTaskView.unlockNavigationDrawer();
        }
    }

    public void onMapReady() {
        String campaign = PreferencesUtil.getInstance().getCurrentCampaignId();
        String operationalArea = PreferencesUtil.getInstance().getCurrentOperationalArea();
        if (StringUtils.isNotBlank(campaign) &&
                StringUtils.isNotBlank(operationalArea)) {
            listTaskInteractor.fetchLocations(campaign, operationalArea);
        } else {
            listTaskView.displayNotification(R.string.select_campaign_operational_area);
            listTaskView.lockNavigationDrawerForSelection();
        }
    }
}
