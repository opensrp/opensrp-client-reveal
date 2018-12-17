package org.smartregister.reveal.widget;

import android.app.Activity;
import android.content.Context;
import android.support.annotation.NonNull;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import com.cocoahero.android.geojson.Feature;
import com.cocoahero.android.geojson.Point;
import com.mapbox.android.gestures.MoveGestureDetector;
import com.mapbox.geojson.Geometry;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.OnMapReadyCallback;
import com.rey.material.util.ViewUtil;
import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interfaces.CommonListener;
import com.vijay.jsonwizard.interfaces.FormWidgetFactory;
import com.vijay.jsonwizard.interfaces.JsonApi;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.reveal.R;

import java.util.ArrayList;
import java.util.List;

import io.ona.kujaku.views.KujakuMapView;

import static org.smartregister.reveal.util.Constants.JsonForm.OPERATIONAL_AREA_TAG;


/**
 * Created by samuelgithengi on 12/13/18.
 */
public class GeoWidgetFactory implements FormWidgetFactory {

    private static final String TAG = "GeoWidgetFactory";

    private static final String ZOOM_LEVEL = "zoom_level";

    @Override
    public List<View> getViewsFromJson(String stepName, Context context, JsonFormFragment formFragment, JSONObject jsonObject, CommonListener listener) throws Exception {
        return getViewsFromJson(stepName, context, formFragment, jsonObject, listener, false);
    }

    @Override
    public List<View> getViewsFromJson(String stepName, Context context, JsonFormFragment formFragment, JSONObject jsonObject, CommonListener listener, boolean popup) throws Exception {
        String openMrsEntityParent = jsonObject.optString(JsonFormConstants.OPENMRS_ENTITY_PARENT);
        String openMrsEntity = jsonObject.optString(JsonFormConstants.OPENMRS_ENTITY);
        String openMrsEntityId = jsonObject.optString(JsonFormConstants.OPENMRS_ENTITY_ID);
        String relevance = jsonObject.optString(JsonFormConstants.RELEVANCE);
        String key = jsonObject.optString(JsonFormConstants.KEY);

        List<View> views = new ArrayList<>(1);

        final int canvasId = ViewUtil.generateViewId();
        View rootLayout = LayoutInflater.from(context)
                .inflate(R.layout.item_geowidget, null);
        rootLayout.setId(canvasId);

        String operationalArea = new JSONObject(formFragment.getCurrentJsonState()).optString(OPERATIONAL_AREA_TAG);

        KujakuMapView mapView = rootLayout.findViewById(R.id.kujakuMapView);
        mapView.onCreate(null);

        mapView.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(MapboxMap mapboxMap) {

                if (operationalArea != null) {
                    mapboxMap.setCameraPosition(mapboxMap.getCameraForGeometry(Geometry.fromJson(operationalArea)));
                }

                writeValues(((JsonApi) context), stepName, getCenterPoint(mapboxMap), key, openMrsEntityParent, openMrsEntity, openMrsEntityId, mapboxMap.getCameraPosition().zoom);
                mapboxMap.addOnMoveListener(new MapboxMap.OnMoveListener() {
                    @Override
                    public void onMoveBegin(@NonNull MoveGestureDetector detector) {//do nothing
                    }

                    @Override
                    public void onMove(@NonNull MoveGestureDetector detector) {//do nothing
                    }

                    @Override
                    public void onMoveEnd(@NonNull MoveGestureDetector detector) {
                        Log.d(TAG, "onMoveEnd: " + mapboxMap.getCameraPosition().target.toString());
                        writeValues(((JsonApi) context), stepName, getCenterPoint(mapboxMap), key,
                                openMrsEntityParent, openMrsEntity, openMrsEntityId, mapboxMap.getCameraPosition().zoom);
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
        if (relevance != null && context instanceof JsonApi) {
            mapView.setTag(com.vijay.jsonwizard.R.id.relevance, relevance);
            ((JsonApi) context).addSkipLogicView(mapView);
        }

        ((JsonApi) context).addFormDataView(mapView);

        int screenHeightPixels = context.getResources().getDisplayMetrics().heightPixels;

        int editTextHeight = context.getResources().getDimensionPixelSize(R.dimen.native_form_edit_text_height);
        mapView.setLayoutParams(new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, screenHeightPixels - editTextHeight));

        views.add(rootLayout);
        mapView.onStart();
        mapView.onResume();

        mapView.showCurrentLocationBtn(true);
        mapView.enableAddPoint(true);
        ((JsonApi) context).onFormFinish();
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
                             String openMrsEntityParent, String openMrsEntity, String openMrsEntityId, double zoomLevel) {
        if (markerPosition == null)
            return;
        try {
            jsonApi.writeValue(stepName, key, markerPosition.toJSON().toString(), openMrsEntityParent, openMrsEntity, openMrsEntityId, false);
            jsonApi.writeValue(stepName, ZOOM_LEVEL, zoomLevel + "", openMrsEntityParent, openMrsEntity, openMrsEntityId, false);
        } catch (JSONException e) {
            Log.e(TAG, "error writing Geowidget values", e);
        }

    }


    private Feature getCenterPoint(MapboxMap mapboxMap) {
        LatLng latLng = mapboxMap.getCameraPosition().target;
        Feature feature = new Feature();
        feature.setGeometry(new Point(latLng.getLatitude(), latLng.getLongitude()));
        return feature;
    }
}
