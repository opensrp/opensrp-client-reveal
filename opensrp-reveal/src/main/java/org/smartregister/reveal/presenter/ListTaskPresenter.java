package org.smartregister.reveal.presenter;

import android.content.Context;
import android.graphics.PointF;
import android.graphics.RectF;
import android.support.annotation.NonNull;
import android.support.v4.util.Pair;
import android.text.TextUtils;
import android.util.Log;

import com.google.gson.reflect.TypeToken;
import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.geojson.Geometry;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONObject;
import org.smartregister.domain.Campaign;
import org.smartregister.domain.Task.TaskStatus;
import org.smartregister.domain.form.FormLocation;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.util.AssetHandler;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import static org.smartregister.AllConstants.REVEAL_OPERATIONAL_AREAS;
import static org.smartregister.reveal.contract.ListTaskContract.ListTaskView;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_SPRAYABLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_SPRAYED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.SPRAYED;
import static org.smartregister.reveal.util.Constants.DETAILS;
import static org.smartregister.reveal.util.Constants.ENTITY_ID;
import static org.smartregister.reveal.util.Constants.GeoJSON.FEATURES;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;

import static org.smartregister.reveal.util.Constants.Map.CLICK_SELECT_RADIUS;
import static org.smartregister.reveal.util.Constants.Map.MAX_SELECT_ZOOM_LEVEL;

import static org.smartregister.reveal.util.Constants.JsonForm.NON_RESIDENTIAL;
import static org.smartregister.reveal.util.Constants.JsonForm.SPRAY_FORM;
import static org.smartregister.reveal.util.Constants.JsonForm.STRUCTURE_PROPERTIES_TYPE;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_TYPE;

import static org.smartregister.reveal.util.Constants.Properties.LOCATION_UUID;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_VERSION;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.Properties.TASK_STATUS;
import static org.smartregister.reveal.util.Constants.Tags.COUNTRY;
import static org.smartregister.reveal.util.Constants.Tags.DISTRICT;
import static org.smartregister.reveal.util.Constants.Tags.HEALTH_CENTER;
import static org.smartregister.reveal.util.Constants.Tags.OPERATIONAL_AREA;
import static org.smartregister.reveal.util.Constants.Tags.PROVINCE;
import static org.smartregister.reveal.util.Utils.getPropertyValue;

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

    private FeatureCollection featureCollection;

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

            if (defaultLocation != null) {
                listTaskView.setDistrict(defaultLocation.get(0));
                listTaskView.setFacility(defaultLocation.get(1));
            }
        } else {
            populateLocationsFromPreferences();
        }

        listTaskView.setCampaign(prefsUtil.getCurrentCampaign());

    }

    public void onShowOperationalAreaSelector() {
        Pair<String, ArrayList<String>> locationHierarchy = extractLocationHierarchy();
        if (locationHierarchy == null) {//try to evict location hierachy in cache
            RevealApplication.getInstance().getContext().anmLocationController().evict();
            locationHierarchy = extractLocationHierarchy();
        }
        if (locationHierarchy != null) {
            listTaskView.showOperationalAreaSelector(extractLocationHierarchy());
        } else {
            listTaskView.displayNotification(R.string.error_fetching_location_hierarchy_title, R.string.error_fetching_location_hierarchy);
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
            List<String> authorizedOperationalAreas = Arrays.asList(StringUtils.split(prefsUtil.getPreferenceValue(REVEAL_OPERATIONAL_AREAS), ','));
            removeUnauthorizedOperationalAreas(authorizedOperationalAreas, entireTree);

            String entireTreeString = AssetHandler.javaToJsonString(entireTree,
                    new TypeToken<List<FormLocation>>() {
                    }.getType());

            return new Pair<>(entireTreeString, new ArrayList<>(defaultLocation));
        } else {
            return null;
        }
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
            featureCollection = FeatureCollection.fromJson(structuresGeoJson.toString());
            listTaskView.setGeoJsonSource(featureCollection, operationalAreaGeometry);
        } else
            listTaskView.displayNotification(R.string.fetching_structures_title, R.string.fetch_structures_failed_message);
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
            listTaskView.displayNotification(R.string.select_campaign_operational_area_title, R.string.select_campaign_operational_area);
//            listTaskView.lockNavigationDrawerForSelection();
        }
    }

    public void onMapClicked(MapboxMap mapboxMap, LatLng point) {
        double currentZoom = mapboxMap.getCameraPosition().zoom;
        if (currentZoom < MAX_SELECT_ZOOM_LEVEL) {
            Log.w(TAG, "onMapClicked Current Zoom level" + currentZoom);
            return;
        }
        final PointF pixel = mapboxMap.getProjection().toScreenLocation(point);
        Context context = listTaskView.getContext();
        List<Feature> features = mapboxMap.queryRenderedFeatures(pixel,
                context.getString(R.string.reveal_layer_polygons), context.getString(R.string.reveal_layer_points));
        if (features.isEmpty()) {//try to increase the click area
            RectF clickArea = new RectF(pixel.x - CLICK_SELECT_RADIUS,
                    pixel.y + CLICK_SELECT_RADIUS, pixel.x + CLICK_SELECT_RADIUS,
                    pixel.y - CLICK_SELECT_RADIUS);
            features = mapboxMap.queryRenderedFeatures(clickArea,
                    context.getString(R.string.reveal_layer_polygons), context.getString(R.string.reveal_layer_points));
            Log.d(TAG, "Selected structure after increasing click area: " + features.size());
            if (features.size() == 1) {
                onFeatureSelected(features.get(0));
            } else {
                Log.d(TAG, "Not Selected structure after increasing click area: " + features.size());
            }
        } else {
            onFeatureSelected(features.get(0));
            if (features.size() > 1) {
                Log.w(TAG, "Selected more than 1 structure: " + features.size());
            }
        }

    }

    private void onFeatureSelected(Feature feature) {
        listTaskView.displaySelectedFeature(feature);
        if (!feature.hasProperty(TASK_IDENTIFIER)) {
            listTaskView.displayNotification(listTaskView.getContext().getString(R.string.task_not_found, feature.id()));
        } else {
            String businessStatus = getPropertyValue(feature, TASK_BUSINESS_STATUS);
            String identifier = getPropertyValue(feature, TASK_IDENTIFIER);
            String code = getPropertyValue(feature, TASK_CODE);
            String status = getPropertyValue(feature, TASK_STATUS);
            if (IRS.equals(code) && NOT_VISITED.equals(businessStatus)) {
                startSprayForm(feature.id(), getPropertyValue(feature, LOCATION_UUID), getPropertyValue(feature, LOCATION_VERSION),
                        getPropertyValue(feature, LOCATION_TYPE),
                        identifier, businessStatus, status);
            } else if (IRS.equals(code) &&
                    (NOT_SPRAYED.equals(businessStatus) || SPRAYED.equals(businessStatus) || NOT_SPRAYABLE.equals(businessStatus))) {
                listTaskInteractor.fetchCardViewDetails(feature.id());
            }
        }
    }

    public void onCardDetailsFetched(CardDetails cardDetails) {
        if (cardDetails == null) {
            return;
        }
        formatCardDetails(cardDetails);
        listTaskView.openCardView(cardDetails);
    }

    private void formatCardDetails(CardDetails cardDetails) {
        // format date
        try {
            DateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");
            Date originalDate = sdf.parse(cardDetails.getSprayDate());

            sdf = new SimpleDateFormat("dd MMM yyyy");
            String formattedDate = sdf.format(originalDate);
            cardDetails.setSprayDate(formattedDate);
        } catch (Exception e) {
            Log.e(TAG, e.getMessage());
        }
        // extract status color
        String sprayStatus = cardDetails.getSprayStatus();
        if ("Not Sprayed".equals(sprayStatus)) {
            cardDetails.setStatusColor("#EE0427");
            cardDetails.setStatusMessage("Sprayable, not sprayed");
        } else if ("Sprayed".equals(sprayStatus)) {
            cardDetails.setStatusColor("#6CBF0F");
            cardDetails.setStatusMessage("Sprayable, sprayed");
        } else {
            cardDetails.setStatusColor("#000000");
            cardDetails.setStatusMessage("Not sprayable");
        }
    }

    private void startSprayForm(String structureId, String structureUUID, String structureVersion, String structureType,
                                String taskIdentifier, String taskBusinessStatus, String taskStatus) {
        try {
            String formString = AssetHandler.readFileFromAssetsFolder(SPRAY_FORM, listTaskView.getContext());
            if (StringUtils.isBlank(structureType)) {
                structureType = NON_RESIDENTIAL;
            }
            formString = formString.replace(STRUCTURE_PROPERTIES_TYPE, structureType);
            JSONObject formJson = new JSONObject(formString);
            formJson.put(ENTITY_ID, structureId);
            JSONObject formData = new JSONObject();
            formData.put(TASK_IDENTIFIER, taskIdentifier);
            formData.put(TASK_BUSINESS_STATUS, taskBusinessStatus);
            formData.put(TASK_STATUS, taskStatus);
            formData.put(LOCATION_UUID, structureUUID);
            formData.put(LOCATION_VERSION, structureVersion);
            formJson.put(DETAILS, formData);
            listTaskView.startSprayForm(formJson);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void saveSprayForm(String json) {
        listTaskView.showProgressDialog();
        listTaskInteractor.saveSprayForm(json);
    }

    @Override
    public void onSprayFormSaved(@NonNull String structureId, @NonNull String taskIdentifier,
                                 @NonNull TaskStatus taskStatus, @NonNull String businessStatus) {
        listTaskView.hideProgressDialog();
        for (Feature feature : featureCollection.features()) {
            if (structureId.equals(feature.id())) {
                feature.addStringProperty(TASK_BUSINESS_STATUS, businessStatus);
                feature.addStringProperty(TASK_STATUS, taskStatus.name());
                break;
            }
        }
        listTaskView.setGeoJsonSource(featureCollection, null);
        listTaskInteractor.fetchCardViewDetails(structureId);
    }
}
