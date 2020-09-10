package org.smartregister.reveal.validators;

import androidx.annotation.NonNull;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.MultiPolygon;
import com.mapbox.geojson.Point;
import com.mapbox.geojson.Polygon;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.turf.TurfJoins;
import com.rengwuxian.materialedittext.validation.METValidator;

import org.smartregister.reveal.R;
import org.smartregister.reveal.util.Constants.Properties;
import org.smartregister.reveal.view.RevealMapView;

import java.util.ArrayList;
import java.util.List;

public class GeoFencingValidator extends METValidator {
    private RevealMapView mapView;
    private Feature operationalArea;
    private boolean disabled = false;
    private int errorId;
    private String[] errorMessageArgs;
    private List<Feature> otherOperationalAreas = new ArrayList<>();

    public GeoFencingValidator(String errorMessage, RevealMapView mapView, Feature operationalArea) {
        super(errorMessage);
        this.mapView = mapView;
        this.operationalArea = operationalArea;
    }

    @Override
    public boolean isValid(@NonNull CharSequence text, boolean isEmpty) {
        if (disabled) return true;
        com.mapbox.geojson.Point selectedPoint = getCenterPoint(mapView.getMapboxMap());
        boolean isWithinOperationArea = inside(selectedPoint, operationalArea);
        if (!isWithinOperationArea) {
            for (Feature feature : otherOperationalAreas) {
                if (inside(selectedPoint, feature)) {
                    errorId = R.string.point_in_another_area;
                    errorMessageArgs = new String[]{feature.getStringProperty(Properties.LOCATION_NAME)};
                    break;
                }
            }
        }
        return isWithinOperationArea;
    }

    private boolean inside(Point selectedPoint, Feature feature) {
        if (feature.geometry() instanceof Polygon) {
            return TurfJoins.inside(selectedPoint, (Polygon) feature.geometry());
        } else if (feature.geometry() instanceof MultiPolygon) {
            return TurfJoins.inside(selectedPoint, (MultiPolygon) feature.geometry());
        }
        return false;
    }

    private com.mapbox.geojson.Point getCenterPoint(MapboxMap mapboxMap) {
        LatLng latLng = mapboxMap.getCameraPosition().target;
        com.mapbox.geojson.Point centerpoint = com.mapbox.geojson.Point.fromLngLat(latLng.getLongitude(), latLng.getLatitude());
        return centerpoint;
    }

    public void setDisabled(boolean disabled) {
        this.disabled = disabled;
    }

    public List<Feature> getOtherOperationalAreas() {
        return otherOperationalAreas;
    }

    public int getErrorId() {
        return errorId;
    }

    public String[] getErrorMessageArgs() {
        return errorMessageArgs;
    }
}
