package org.smartregister.reveal.validators;

import androidx.annotation.NonNull;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.MultiPolygon;
import com.mapbox.geojson.Polygon;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.turf.TurfJoins;
import com.rengwuxian.materialedittext.validation.METValidator;

import org.smartregister.reveal.view.RevealMapView;

public class WithinOperationAreaValidator extends METValidator {
    private RevealMapView mapView;
    private Feature operationalArea;
    private boolean disabled=false;

    public WithinOperationAreaValidator(String errorMessage, RevealMapView mapView, Feature operationalArea) {
        super(errorMessage);
        this.mapView = mapView;
        this.operationalArea = operationalArea;
    }

    @Override
    public boolean isValid(@NonNull CharSequence text, boolean isEmpty) {
        if(disabled) return true;
        com.mapbox.geojson.Point selectedpoint = getCenterPoint(mapView.getMapboxMap());
        boolean isWithinOperationArea = TurfJoins.inside(selectedpoint, MultiPolygon.fromPolygon((Polygon) operationalArea.geometry()));
        return isWithinOperationArea;
    }

    private com.mapbox.geojson.Point getCenterPoint(MapboxMap mapboxMap) {
        LatLng latLng = mapboxMap.getCameraPosition().target;
        com.mapbox.geojson.Point  centerpoint = com.mapbox.geojson.Point.fromLngLat(latLng.getLongitude(), latLng.getLatitude());
        return centerpoint;
    }

    public void setDisabled(boolean disabled) {
        this.disabled = disabled;
    }
}
