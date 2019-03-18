package org.smartregister.reveal;

import android.content.Context;
import android.location.Location;
import android.support.annotation.NonNull;
import android.support.v4.util.Pair;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.geojson.Geometry;
import com.mapbox.mapboxsdk.geometry.LatLng;

import org.json.JSONObject;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.model.CardDetails;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Vincent Karuri
 */
public class TestListTaskView implements ListTaskContract.ListTaskView {

    @Override
    public Location getUserCurrentLocation() {
        return null;
    }

    @Override
    public void showProgressDialog(int title, int message) {

    }

    @Override
    public void hideProgressDialog() {

    }

    @Override
    public void requestUserLocation() {

    }

    @Override
    public Context getContext() {
        return null;
    }

    @Override
    public void closeStructureCardView() {

    }

    @Override
    public void showOperationalAreaSelector(Pair<String, ArrayList<String>> locationHierarchy) {

    }

    @Override
    public void setCampaign(String campaign) {

    }

    @Override
    public void setOperationalArea(String operationalArea) {

    }

    @Override
    public void setDistrict(String district) {

    }

    @Override
    public void setFacility(String facility) {

    }

    @Override
    public void setOperator() {

    }

    @Override
    public void showCampaignSelector(List<String> campaigns, String entireTreeString) {

    }

    @Override
    public void setGeoJsonSource(@NonNull FeatureCollection featureCollection, Geometry operationalAreaGeometry) {

    }

    @Override
    public void lockNavigationDrawerForSelection() {

    }

    @Override
    public void unlockNavigationDrawer() {

    }

    @Override
    public void displayNotification(int title, int message, Object... formatArgs) {

    }

    @Override
    public void displayNotification(String message) {

    }

    @Override
    public void openCardView(CardDetails cardDetails) {

    }

    @Override
    public void startJsonForm(JSONObject form) {

    }

    @Override
    public void displaySelectedFeature(Feature feature, LatLng clickedPoint) {

    }

    @Override
    public void clearSelectedFeature() {

    }

    @Override
    public void displayToast(int resourceId) {

    }
}
