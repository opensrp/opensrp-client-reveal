package org.smartregister.reveal.widget;

import android.app.Activity;
import android.content.Context;
import android.graphics.Color;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.Toast;

import com.cocoahero.android.geojson.Feature;
import com.cocoahero.android.geojson.Point;
import com.mapbox.android.core.permissions.PermissionsManager;
import com.mapbox.android.gestures.MoveGestureDetector;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.mapboxsdk.camera.CameraPosition;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.location.LocationComponent;
import com.mapbox.mapboxsdk.location.modes.RenderMode;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.OnMapReadyCallback;
import com.mapbox.mapboxsdk.maps.Style;
import com.mapbox.mapboxsdk.style.sources.GeoJsonSource;
import com.rengwuxian.materialedittext.validation.METValidator;
import com.rey.material.util.ViewUtil;
import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interfaces.CommonListener;
import com.vijay.jsonwizard.interfaces.FormWidgetFactory;
import com.vijay.jsonwizard.interfaces.JsonApi;
import com.vijay.jsonwizard.interfaces.LifeCycleListener;
import com.vijay.jsonwizard.utils.ValidationStatus;
import com.vijay.jsonwizard.views.JsonFormFragmentView;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants.Map;
import org.smartregister.reveal.util.RevealMapHelper;
import org.smartregister.reveal.validators.MinZoomValidator;
import org.smartregister.reveal.view.RevealMapView;
import org.smartregister.util.AssetHandler;
import org.smartregister.util.Utils;

import java.util.ArrayList;
import java.util.List;

import io.ona.kujaku.callbacks.OnLocationComponentInitializedCallback;
import io.ona.kujaku.layers.BoundaryLayer;
import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DIGITAL_GLOBE_CONNECT_ID;
import static org.smartregister.reveal.util.Constants.JsonForm.LOCATION_COMPONENT_ACTIVE;
import static org.smartregister.reveal.util.Constants.JsonForm.OPERATIONAL_AREA_TAG;
import static org.smartregister.reveal.util.Constants.JsonForm.STRUCTURES_TAG;
import static org.smartregister.reveal.util.Utils.getLocationBuffer;
import static org.smartregister.reveal.util.Utils.getPixelsPerDPI;

/**
 * Created by samuelgithengi on 12/13/18.
 */
public class GeoWidgetFactory implements FormWidgetFactory, LifeCycleListener, OnLocationComponentInitializedCallback {

    private static final String TAG = "GeoWidgetFactory";

    private static final String ZOOM_LEVEL = "zoom_level";

    private static final String MAX_ZOOM_LEVEL = "v_zoom_max";

    private RevealMapView mapView;

    private JsonApi jsonApi;

    private ImageButton myLocationButton;

    private RevealMapHelper mapHelper = new RevealMapHelper();

    public static ValidationStatus validate(JsonFormFragmentView formFragmentView, RevealMapView mapView) {

        if (!Utils.isEmptyCollection(mapView.getValidators())) {
            for (METValidator validator : mapView.getValidators()) {
                if (validator instanceof MinZoomValidator) {
                    Double zoom = mapView.getMapboxMapZoom();
                    if (zoom != null && !validator.isValid(String.valueOf(zoom), false)) {
                        Toast.makeText(formFragmentView.getContext(), validator.getErrorMessage(), Toast.LENGTH_LONG).show();
                        return new ValidationStatus(false, validator.getErrorMessage(), formFragmentView, mapView);
                    }
                }
            }
        }
        return new ValidationStatus(true, null, null, null);
    }

    @Override
    public List<View> getViewsFromJson(String stepName, Context context, JsonFormFragment formFragment, JSONObject jsonObject, CommonListener listener) throws Exception {
        return getViewsFromJson(stepName, context, formFragment, jsonObject, listener, false);
    }

    @Override
    public List<View> getViewsFromJson(String stepName, Context context, JsonFormFragment formFragment, JSONObject jsonObject, CommonListener listener, boolean popup) throws Exception {
        jsonApi = ((JsonApi) context);
        jsonApi.registerLifecycleListener(this);
        String openMrsEntityParent = jsonObject.optString(JsonFormConstants.OPENMRS_ENTITY_PARENT);
        String openMrsEntity = jsonObject.optString(JsonFormConstants.OPENMRS_ENTITY);
        String openMrsEntityId = jsonObject.optString(JsonFormConstants.OPENMRS_ENTITY_ID);
        String relevance = jsonObject.optString(JsonFormConstants.RELEVANCE);
        String key = jsonObject.optString(JsonFormConstants.KEY);


        List<View> views = new ArrayList<>(1);

        final int canvasId = ViewUtil.generateViewId();
        mapView = (RevealMapView) LayoutInflater.from(context)
                .inflate(R.layout.item_geowidget, null);

        String operationalArea = null;
        String featureCollection = null;
        boolean locationComponentActive = false;

        try {
            operationalArea = new JSONObject(formFragment.getCurrentJsonState()).optString(OPERATIONAL_AREA_TAG);
            featureCollection = new JSONObject(formFragment.getCurrentJsonState()).optString(STRUCTURES_TAG);
            locationComponentActive = new JSONObject(formFragment.getCurrentJsonState()).optBoolean(LOCATION_COMPONENT_ACTIVE);
        } catch (JSONException e) {
            Timber.e(e, "error extracting geojson form jsonform");
        }

        mapView.setId(canvasId);
        mapView.onCreate(null);
        mapView.setDisableMyLocationOnMapMove(true);
        mapView.getMapboxLocationComponentWrapper().setOnLocationComponentInitializedCallback(this);


        myLocationButton = mapView.findViewById(R.id.ib_mapview_focusOnMyLocationIcon);

        com.mapbox.geojson.Feature operationalAreaFeature = null;
        if (operationalArea != null) {
            operationalAreaFeature = com.mapbox.geojson.Feature.fromJson(operationalArea);
            BoundaryLayer.Builder boundaryBuilder = new BoundaryLayer.Builder(FeatureCollection.fromFeature(operationalAreaFeature))
                    .setLabelProperty(Map.NAME_PROPERTY)
                    .setLabelTextSize(context.getResources().getDimension(R.dimen.operational_area_boundary_text_size))
                    .setLabelColorInt(Color.WHITE)
                    .setBoundaryColor(Color.WHITE)
                    .setBoundaryWidth(context.getResources().getDimension(R.dimen.operational_area_boundary_width));
            mapView.addLayer(boundaryBuilder.build());
        }


        String finalFeatureCollection = featureCollection;
        com.mapbox.geojson.Feature finalOperationalAreaFeature = operationalAreaFeature;

        boolean finalLocationComponentActive = locationComponentActive;
        mapView.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(@NonNull MapboxMap mapboxMap) {

                String mapBoxStyle = AssetHandler.readFileFromAssetsFolder(context.getString(R.string.reveal_satellite_style), context);
                Style.Builder builder = new Style.Builder().fromJson(mapBoxStyle.replace(DIGITAL_GLOBE_CONNECT_ID, BuildConfig.DG_CONNECT_ID));

                mapboxMap.setStyle(builder, new Style.OnStyleLoaded() {
                    @Override
                    public void onStyleLoaded(@NonNull Style style) {
                        GeoJsonSource geoJsonSource = style.getSourceAs(context.getString(R.string.reveal_datasource_name));

                        if (geoJsonSource != null && StringUtils.isNotBlank(finalFeatureCollection)) {
                            geoJsonSource.setGeoJson(finalFeatureCollection);
                        }
                        RevealMapHelper.addCustomLayers(style, context);
                        mapView.setMapboxMap(mapboxMap);
                    }
                });

                mapboxMap.getUiSettings().setRotateGesturesEnabled(false);

                mapView.setMapboxMap(mapboxMap);
                float bufferRadius = getLocationBuffer() / getPixelsPerDPI(context.getResources());
                mapView.setLocationBufferRadius(bufferRadius);


                if (finalOperationalAreaFeature != null && !finalLocationComponentActive) {
                    CameraPosition cameraPosition = mapboxMap.getCameraForGeometry(finalOperationalAreaFeature.geometry());
                    if (cameraPosition != null) {
                        mapboxMap.setCameraPosition(cameraPosition);
                    }
                } else {
                    mapView.focusOnUserLocation(true, bufferRadius);
                }

                writeValues(((JsonApi) context), stepName, getCenterPoint(mapboxMap), key, openMrsEntityParent, openMrsEntity, openMrsEntityId, mapboxMap.getCameraPosition().zoom, finalLocationComponentActive);
                mapboxMap.addOnMoveListener(new MapboxMap.OnMoveListener() {
                    @Override
                    public void onMoveBegin(@NonNull MoveGestureDetector detector) {//do nothing
                    }

                    @Override
                    public void onMove(@NonNull MoveGestureDetector detector) {//do nothing
                    }

                    @Override
                    public void onMoveEnd(@NonNull MoveGestureDetector detector) {
                        Timber.d("onMoveEnd: " + mapboxMap.getCameraPosition().target.toString());
                        writeValues(((JsonApi) context), stepName, getCenterPoint(mapboxMap), key,
                                openMrsEntityParent, openMrsEntity, openMrsEntityId,
                                mapboxMap.getCameraPosition().zoom,
                                mapHelper.isMyLocationComponentActive(context, myLocationButton));
                    }
                });
            }
        });

        JSONArray canvasIdsArray = new JSONArray();
        canvasIdsArray.put(canvasId);
        mapView.setTag(com.vijay.jsonwizard.R.id.canvas_ids, canvasIdsArray.toString());
        mapView.setTag(com.vijay.jsonwizard.R.id.address, stepName + ":" + jsonObject.getString(JsonFormConstants.KEY));
        mapView.setTag(com.vijay.jsonwizard.R.id.key, jsonObject.getString(JsonFormConstants.KEY));
        mapView.setTag(com.vijay.jsonwizard.R.id.openmrs_entity_parent, openMrsEntityParent);
        mapView.setTag(com.vijay.jsonwizard.R.id.openmrs_entity, openMrsEntity);
        mapView.setTag(com.vijay.jsonwizard.R.id.openmrs_entity_id, openMrsEntityId);
        mapView.setTag(com.vijay.jsonwizard.R.id.type, jsonObject.getString(JsonFormConstants.TYPE));
        if (relevance != null) {
            mapView.setTag(com.vijay.jsonwizard.R.id.relevance, relevance);
            ((JsonApi) context).addSkipLogicView(mapView);
        }

        ((JsonApi) context).addFormDataView(mapView);

        mapView.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, 0, 1));

        addMaximumZoomLevel(jsonObject, mapView);
        views.add(mapView);
        mapView.onStart();
        mapView.showCurrentLocationBtn(true);
        mapView.enableAddPoint(true);
        disableParentScroll((Activity) context, mapView);
        return views;
    }

    private void disableParentScroll(Activity context, View mapView) {
        ViewGroup mainScroll = context.findViewById(R.id.scroll_view);
        mapView.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(View v, MotionEvent event) {
                mainScroll.requestDisallowInterceptTouchEvent(true);
                return false;
            }
        });

    }

    private void writeValues(JsonApi jsonApi, String stepName, Feature markerPosition, String key,
                             String openMrsEntityParent, String openMrsEntity, String openMrsEntityId, double zoomLevel, boolean finalLocationComponentActive) {
        if (markerPosition == null)
            return;
        try {
            jsonApi.writeValue(stepName, key, markerPosition.toJSON().toString(), openMrsEntityParent, openMrsEntity, openMrsEntityId, false);
            jsonApi.writeValue(stepName, ZOOM_LEVEL, zoomLevel + "", openMrsEntityParent, openMrsEntity, openMrsEntityId, false);
            jsonApi.writeValue(stepName, LOCATION_COMPONENT_ACTIVE, finalLocationComponentActive + "", openMrsEntityParent, openMrsEntity, openMrsEntityId, false);
        } catch (JSONException e) {
            Timber.e(e, "error writing Geowidget values");
        }

    }


    private Feature getCenterPoint(MapboxMap mapboxMap) {
        LatLng latLng = mapboxMap.getCameraPosition().target;
        Feature feature = new Feature();
        feature.setGeometry(new Point(latLng.getLatitude(), latLng.getLongitude()));
        return feature;
    }

    private void addMaximumZoomLevel(JSONObject jsonObject, RevealMapView mapView) {

        JSONObject minValidation = jsonObject.optJSONObject(MAX_ZOOM_LEVEL);
        if (minValidation != null) {
            try {
                mapView.addValidator(new MinZoomValidator(minValidation.getString(JsonFormConstants.ERR),
                        minValidation.getDouble(JsonFormConstants.VALUE)));
            } catch (JSONException e) {
                Timber.e("Error extracting max zoom level from" + minValidation);
            }
        }
    }

    @Override
    public void onLocationComponentInitialized() {
        if (PermissionsManager.areLocationPermissionsGranted(mapView.getContext())) {
            LocationComponent locationComponent = mapView.getMapboxLocationComponentWrapper()
                    .getLocationComponent();
            locationComponent.applyStyle(mapView.getContext(), R.style.LocationComponentStyling);
            locationComponent.setRenderMode(RenderMode.COMPASS);
        }
    }

    @Override
    public void onCreate(Bundle bundle) {
        if (mapView != null) {
            mapView.onCreate(bundle);
        }
    }

    @Override
    public void onStart() {
        if (mapView != null) {
            mapView.onStart();
        }
    }

    @Override
    public void onResume() {
        if (mapView != null)
            mapView.onResume();
    }

    @Override
    public void onPause() {
        if (myLocationButton != null && jsonApi instanceof Context)
            RevealApplication.getInstance().setMyLocationComponentEnabled(mapHelper.isMyLocationComponentActive((Context) jsonApi, myLocationButton));
        if (mapView != null)
            mapView.onPause();
    }

    @Override
    public void onStop() {
        if (mapView != null)
            mapView.onStop();
    }

    @Override
    public void onSaveInstanceState(Bundle bundle) {
        if (mapView != null)
            mapView.onSaveInstanceState(bundle);
    }

    @Override
    public void onLowMemory() {
        if (mapView != null)
            mapView.onLowMemory();
    }

    @Override
    public void onDestroy() {
        jsonApi.unregisterLifecycleListener(this);
    }
}
