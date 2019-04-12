package org.smartregister.reveal.presenter;

import android.content.Context;
import android.graphics.PointF;
import android.graphics.RectF;
import android.location.Location;
import android.support.annotation.NonNull;
import android.support.annotation.StringRes;
import android.support.v7.app.AlertDialog;
import android.util.Log;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.domain.Task.TaskStatus;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.contract.PasswordRequestCallback;
import org.smartregister.reveal.contract.UserLocationContract.UserLocationCallback;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.MosquitoCollectionCardDetails;
import org.smartregister.reveal.model.SprayCardDetails;
import org.smartregister.reveal.util.CardDetailsUtil;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.PasswordDialogUtils;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.util.AssetHandler;
import org.smartregister.util.Utils;

import java.util.List;

import static org.smartregister.reveal.contract.ListTaskContract.ListTaskView;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.INCOMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.IN_PROGRESS;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_SPRAYABLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_SPRAYED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.SPRAYED;
import static org.smartregister.reveal.util.Constants.DateFormat.EVENT_DATE_FORMAT_XXX;
import static org.smartregister.reveal.util.Constants.DateFormat.EVENT_DATE_FORMAT_Z;
import static org.smartregister.reveal.util.Constants.GeoJSON.FEATURES;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.JsonForm.ADD_STRUCTURE_FORM;
import static org.smartregister.reveal.util.Constants.JsonForm.OPERATIONAL_AREA_TAG;
import static org.smartregister.reveal.util.Constants.JsonForm.STRUCTURES_TAG;
import static org.smartregister.reveal.util.Constants.MOSQUITO_COLLECTION_EVENT;
import static org.smartregister.reveal.util.Constants.Map.CLICK_SELECT_RADIUS;
import static org.smartregister.reveal.util.Constants.Map.MAX_SELECT_ZOOM_LEVEL;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.Properties.TASK_STATUS;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;
import static org.smartregister.reveal.util.Utils.formatDate;
import static org.smartregister.reveal.util.Utils.getPropertyValue;


/**
 * Created by samuelgithengi on 11/27/18.
 */
public class ListTaskPresenter implements ListTaskContract.Presenter, PasswordRequestCallback,
        UserLocationCallback {

    private static final String TAG = "ListTaskPresenter";

    private ListTaskView listTaskView;

    private ListTaskInteractor listTaskInteractor;

    private PreferencesUtil prefsUtil;

    private FeatureCollection featureCollection;

    private Feature operationalArea;

    private Feature selectedFeature;

    private String selectedFeatureInterventionType;

    private LatLng clickedPoint;

    private AlertDialog passwordDialog;

    private ValidateUserLocationPresenter locationPresenter;

    private SprayCardDetails sprayCardDetails;

    private MosquitoCollectionCardDetails mosquitoCollectionCardDetails;

    private boolean changeSprayStatus;

    private boolean changeMosquitoCollectionStatus;

    private BaseDrawerContract.Presenter drawerPresenter;

    public ListTaskPresenter(ListTaskView listTaskView, BaseDrawerContract.Presenter drawerPresenter) {

        this.listTaskView = listTaskView;
        this.drawerPresenter = drawerPresenter;
        listTaskInteractor = new ListTaskInteractor(this);
        passwordDialog = PasswordDialogUtils.initPasswordDialog(listTaskView.getContext(), this);
        locationPresenter = new ValidateUserLocationPresenter(listTaskView, this);
        prefsUtil = PreferencesUtil.getInstance();
    }

    @Override
    public void onDrawerClosed() {
        if (drawerPresenter.isChangedCurrentSelection()) {
            listTaskView.showProgressDialog(R.string.fetching_structures_title, R.string.fetching_structures_message);
            listTaskInteractor.fetchLocations(prefsUtil.getCurrentCampaignId(), prefsUtil.getCurrentOperationalArea());
        }
    }

    public void refreshStructures() {
        listTaskView.showProgressDialog(R.string.fetching_structures_title, R.string.fetching_structures_message);
        listTaskInteractor.fetchLocations(prefsUtil.getCurrentCampaignId(), prefsUtil.getCurrentOperationalArea());
    }

    @Override
    public void onStructuresFetched(JSONObject structuresGeoJson, Feature operationalArea) {
        listTaskView.hideProgressDialog();
        drawerPresenter.setChangedCurrentSelection(false);
        if (structuresGeoJson.has(FEATURES)) {
            featureCollection = FeatureCollection.fromJson(structuresGeoJson.toString());
            listTaskView.setGeoJsonSource(featureCollection, operationalArea);
            this.operationalArea = operationalArea;
            if (Utils.isEmptyCollection(featureCollection.features())) {
                listTaskView.displayNotification(R.string.fetching_structures_title, R.string.no_structures_found);
            }
        } else {
            listTaskView.displayNotification(R.string.fetching_structures_title,
                    R.string.fetch_location_and_structures_failed, prefsUtil.getCurrentOperationalArea());
            try {
                structuresGeoJson.put(FEATURES, new JSONArray());
                listTaskView.setGeoJsonSource(FeatureCollection.fromJson(structuresGeoJson.toString()), operationalArea);
                listTaskView.clearSelectedFeature();
                listTaskView.closeCardView(R.id.btn_collapse_spray_card_view);
            } catch (JSONException e) {
                Log.e(TAG, "error resetting structures");
            }
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
            drawerPresenter.getView().lockNavigationDrawerForSelection();
        }
    }

    public void onMapClicked(MapboxMap mapboxMap, LatLng point) {
        double currentZoom = mapboxMap.getCameraPosition().zoom;
        if (currentZoom < MAX_SELECT_ZOOM_LEVEL) {
            Log.w(TAG, "onMapClicked Current Zoom level" + currentZoom);
            listTaskView.displayToast(R.string.zoom_in_to_select);
            return;
        }
        clickedPoint = point;
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
        this.selectedFeature = feature;
        this.changeSprayStatus = false;
        this.changeMosquitoCollectionStatus = false;

        listTaskView.closeCardView(R.id.btn_collapse_mosquito_collection_card_view);
        listTaskView.displaySelectedFeature(feature, clickedPoint);
        if (!feature.hasProperty(TASK_IDENTIFIER)) {
            listTaskView.displayNotification(listTaskView.getContext().getString(R.string.task_not_found, prefsUtil.getCurrentOperationalArea()));
        } else {
            String businessStatus = getPropertyValue(feature, TASK_BUSINESS_STATUS);
            String code = getPropertyValue(feature, TASK_CODE);
            selectedFeatureInterventionType = code;
            if ((IRS.equals(code) || MOSQUITO_COLLECTION.equals(code)) && NOT_VISITED.equals(businessStatus)) {
                if (BuildConfig.VALIDATE_FAR_STRUCTURES) {
                    validateUserLocation();
                } else {
                    onLocationValidated();
                }
            } else if (IRS.equals(code) &&
                    (NOT_SPRAYED.equals(businessStatus) || SPRAYED.equals(businessStatus) || NOT_SPRAYABLE.equals(businessStatus))) {
                listTaskInteractor.fetchSprayDetails(feature.id(), false);
            } else if (MOSQUITO_COLLECTION.equals(code)
                    && (INCOMPLETE.equals(businessStatus) || IN_PROGRESS.equals(businessStatus)
                    || NOT_ELIGIBLE.equals(businessStatus) || COMPLETE.equals(businessStatus))) {
                listTaskInteractor.fetchMosquitoCollectionDetails(feature.id(), false);
            }
        }
    }

    private void validateUserLocation() {
        Location location = listTaskView.getUserCurrentLocation();
        if (location == null) {
            locationPresenter.requestUserLocation();
        } else {
            locationPresenter.onGetUserLocation(location);
        }
    }


    @Override
    public void onInterventionFormDetailsFetched(CardDetails cardDetails) {
        if (cardDetails instanceof SprayCardDetails) {
            this.sprayCardDetails = (SprayCardDetails) cardDetails;
            this.changeSprayStatus = true;
        } else if (cardDetails instanceof MosquitoCollectionCardDetails) {
            this.mosquitoCollectionCardDetails = (MosquitoCollectionCardDetails) cardDetails;
            this.changeMosquitoCollectionStatus = true;
        }
        listTaskView.hideProgressDialog();
        validateUserLocation();
    }

    @Override
    public void onCardDetailsFetched(CardDetails cardDetails) {
        if (cardDetails instanceof SprayCardDetails) {
            if (cardDetails == null) {
                return;
            }
            formatSprayCardDetails((SprayCardDetails) cardDetails);
            listTaskView.openCardView(cardDetails);
        } else if (cardDetails instanceof MosquitoCollectionCardDetails) {
            listTaskView.openCardView(cardDetails);
        }
    }

    private void formatSprayCardDetails(SprayCardDetails sprayCardDetails) {
        try {
            // format date
            String formattedDate = formatDate(sprayCardDetails.getSprayDate(), EVENT_DATE_FORMAT_Z);
            sprayCardDetails.setSprayDate(formattedDate);
        } catch (Exception e) {
            Log.e(TAG, e.getMessage());
            Log.i(TAG, "Date parsing failed, trying another date format");
            try {
                // try another date format
                String formattedDate = formatDate(sprayCardDetails.getSprayDate(), EVENT_DATE_FORMAT_XXX);
                sprayCardDetails.setSprayDate(formattedDate);
            } catch (Exception exception) {
                Log.e(TAG, exception.getMessage());
            }
        }

        CardDetailsUtil.formatCardDetails(sprayCardDetails);
    }

    private void startForm(Feature feature, CardDetails cardDetails, String encounterType) {
        String formName = RevealJsonFormUtils.getFormName(encounterType, null);
        String sprayStatus = cardDetails == null ? null : cardDetails.getStatus();
        String familyHead = null;
        if (cardDetails instanceof SprayCardDetails) {
            familyHead = ((SprayCardDetails) cardDetails).getFamilyHead();
        }
        startForm(formName, feature, sprayStatus, familyHead);
    }

    private void startForm(String formName, Feature feature, String sprayStatus, String familyHead) {
        JSONObject formJson = listTaskView.getJsonFormUtils().getFormJSON(listTaskView.getContext()
                , formName, feature, sprayStatus, familyHead);
        listTaskView.startJsonForm(formJson);
    }

    public void onChangeInterventionStatus(String interventionType) {
        if (IRS.equals(interventionType)) {
            listTaskView.showProgressDialog(R.string.fetching_structure_title, R.string.fetching_structure_message);
            listTaskInteractor.fetchSprayDetails(selectedFeature.id(), true);
        } else if (MOSQUITO_COLLECTION.equals(interventionType)) {
            listTaskView.showProgressDialog(R.string.fetching_mosquito_collection_points_title, R.string.fetching_mosquito_collection_points_message);
            listTaskInteractor.fetchMosquitoCollectionDetails(selectedFeature.id(), true);
        }
    }

    public void saveJsonForm(String json) {
        listTaskView.showProgressDialog(R.string.saving_title, R.string.saving_message);
        listTaskInteractor.saveJsonForm(json);
    }

    @Override
    public void onFormSaved(@NonNull String structureId, @NonNull TaskStatus taskStatus, @NonNull String businessStatus, String interventionType) {
        listTaskView.hideProgressDialog();
        for (Feature feature : featureCollection.features()) {
            if (structureId.equals(feature.id())) {
                feature.addStringProperty(TASK_BUSINESS_STATUS, businessStatus);
                feature.addStringProperty(TASK_STATUS, taskStatus.name());
                break;
            }
        }
        listTaskView.setGeoJsonSource(featureCollection, null);

        if (IRS.equals(interventionType)) {
            listTaskInteractor.fetchSprayDetails(structureId, false);
        } else if (MOSQUITO_COLLECTION.equals(interventionType)) {
            listTaskInteractor.fetchMosquitoCollectionDetails(structureId, false);
        }
    }

    @Override
    public void onStructureAdded(Feature feature, JSONArray featureCoordinates) {
        listTaskView.hideProgressDialog();
        featureCollection.features().add(feature);
        listTaskView.setGeoJsonSource(featureCollection, null);
        try {
            clickedPoint = new LatLng(featureCoordinates.getDouble(1), featureCoordinates.getDouble(0));
            listTaskView.displaySelectedFeature(feature, clickedPoint);

        } catch (JSONException e) {
            Log.e(TAG, "error extracting coordinates of added structure", e);
        }

    }

    @Override
    public void onFormSaveFailure(String eventType) {
        listTaskView.hideProgressDialog();
        listTaskView.displayNotification(R.string.form_save_failure_title,
                eventType.equals(SPRAY_EVENT) ? R.string.spray_form_save_failure : R.string.add_structure_form_save_failure);
    }


    public void onAddStructureClicked() {
        String formString = AssetHandler.readFileFromAssetsFolder(ADD_STRUCTURE_FORM, listTaskView.getContext());
        try {
            JSONObject formJson = new JSONObject(formString);
            formJson.put(OPERATIONAL_AREA_TAG, operationalArea.toJson());
            formJson.put(STRUCTURES_TAG, featureCollection.toJson());
            listTaskView.startJsonForm(formJson);
        } catch (Exception e) {
            Log.e(TAG, "error launching add structure form", e);
        }

    }

    @Override
    public void onLocationValidated() {
        if (IRS.equals(selectedFeatureInterventionType)) {
            if (sprayCardDetails == null || !changeSprayStatus) {
                startForm(selectedFeature, null, SPRAY_EVENT);
            } else {
                startForm(selectedFeature, sprayCardDetails, SPRAY_EVENT);
            }
        } else if (MOSQUITO_COLLECTION.equals(selectedFeatureInterventionType)) {
            if (mosquitoCollectionCardDetails == null || !changeMosquitoCollectionStatus) {
                startForm(selectedFeature, null, MOSQUITO_COLLECTION_EVENT);
            } else {
                startForm(selectedFeature, mosquitoCollectionCardDetails, MOSQUITO_COLLECTION_EVENT);
            }
        }
    }

    @Override
    public void onPasswordVerified() {
        onLocationValidated();
    }

    @Override
    public LatLng getTargetCoordinates() {
        return clickedPoint;
    }

    @Override
    public void requestUserPassword() {
        if (passwordDialog != null) {
            passwordDialog.show();
        }
    }

    @Override
    public ValidateUserLocationPresenter getLocationPresenter() {
        return locationPresenter;
    }

    @Override
    public Feature getSelectedFeature() {
        return selectedFeature;
    }

    @Override
    public int getInterventionLabel() {
        String campaignId = prefsUtil.getCurrentCampaignId();
        String intervention = campaignId.substring(0, campaignId.indexOf("_"));
        if (Constants.Intervention.IRS.equals(intervention))
            return R.string.irs;
        else
            return R.string.focus_investigation;
    }
}
