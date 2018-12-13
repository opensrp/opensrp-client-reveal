package org.smartregister.reveal.widget;

import android.app.Activity;
import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;

import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.OnMapReadyCallback;
import com.rey.material.util.ViewUtil;
import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interfaces.CommonListener;
import com.vijay.jsonwizard.interfaces.FormWidgetFactory;
import com.vijay.jsonwizard.interfaces.JsonApi;

import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.reveal.R;

import java.util.ArrayList;
import java.util.List;

import io.ona.kujaku.views.KujakuMapView;


/**
 * Created by samuelgithengi on 12/13/18.
 */
public class GeoWidgetFactory implements FormWidgetFactory {

    private static final String TAG = "GeoWidgetFactory";

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

        List<View> views = new ArrayList<>(1);

        final int canvasId = ViewUtil.generateViewId();
        View rootLayout = LayoutInflater.from(context)
                .inflate(R.layout.item_geowidget, null);
        rootLayout.setId(canvasId);

        KujakuMapView mapView = rootLayout.findViewById(R.id.kujakuMapView);
        mapView.onCreate(null);

        mapView.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(MapboxMap mapboxMap) {
                Log.d(TAG, mapboxMap.getCameraPosition().target.toString());
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

        views.add(rootLayout);
        mapView.onStart();
        mapView.onResume();

        mapView.showCurrentLocationBtn(true);
        mapView.focusOnUserLocation(true);
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
}
