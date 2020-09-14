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

import static org.smartregister.reveal.widget.GeoWidgetFactory.OTHER;

public class GeoFencingValidator extends METValidator {
    private RevealMapView mapView;
    private Feature operationalArea;
    private boolean disabled = false;
    private int errorId;
    private String[] errorMessageArgs;
    private List<Feature> operationalAreas = new ArrayList<>();
    private String selectedOperationalArea;
    private Feature otherOperationalArea;
    private boolean operationalAreaOther;

    public GeoFencingValidator(String errorMessage, RevealMapView mapView, Feature operationalArea) {
        super(errorMessage);
        this.mapView = mapView;
        this.operationalArea = operationalArea;
        if (operationalArea.hasProperty(Properties.LOCATION_NAME) &&
                operationalArea.getStringProperty(Properties.LOCATION_NAME).toLowerCase().contains(OTHER)) {
            otherOperationalArea = operationalArea;
            operationalAreaOther = true;
        }
    }

    @Override
    public boolean isValid(@NonNull CharSequence text, boolean isEmpty) {
        if (disabled) return true;
        Point selectedPoint = getCenterPoint(mapView.getMapboxMap());
        boolean isWithinOperationArea = inside(selectedPoint, operationalArea);
        boolean validOperationalFound = false;
        if (!isWithinOperationArea) {
            for (Feature feature : operationalAreas) {
                if (inside(selectedPoint, feature)) {
                    errorId = R.string.point_within_known_operational_area;
                    errorMessageArgs = new String[]{feature.getStringProperty(Properties.LOCATION_NAME)};
                    selectedOperationalArea = errorMessageArgs[0];
                    validOperationalFound = true;
                    break;
                }
            }
        } else {
            errorId = 0;
            errorMessageArgs = null;
            selectedOperationalArea = null;
        }
        if (isOperationalAreaOther()) {
            errorId = R.string.point_not_within_other_operational_area;
            errorMessageArgs = new String[]{otherOperationalArea.getStringProperty(Properties.LOCATION_NAME)};
            selectedOperationalArea = errorMessageArgs[0];
        } else if (!isWithinOperationArea && !validOperationalFound && otherOperationalArea != null) {
            errorId = R.string.point_not_within_known_operational_area;
            errorMessageArgs = new String[]{otherOperationalArea.getStringProperty(Properties.LOCATION_NAME)};
            selectedOperationalArea = errorMessageArgs[0];
        } else if (!isWithinOperationArea && !validOperationalFound) {
            errorId = R.string.other_operational_area_not_defined;
            errorMessageArgs = new String[]{operationalArea.getStringProperty(Properties.LOCATION_NAME)};
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

    private Point getCenterPoint(MapboxMap mapboxMap) {
        LatLng latLng = mapboxMap.getCameraPosition().target;
        Point centerpoint = Point.fromLngLat(latLng.getLongitude(), latLng.getLatitude());
        return centerpoint;
    }

    public void setDisabled(boolean disabled) {
        this.disabled = disabled;
    }

    public List<Feature> getOperationalAreas() {
        return operationalAreas;
    }

    public int getErrorId() {
        return errorId;
    }

    public String[] getErrorMessageArgs() {
        return errorMessageArgs;
    }

    public String getSelectedOperationalArea() {
        return selectedOperationalArea;
    }

    public void setOtherOperationalArea(Feature otherOperationalArea) {
        this.otherOperationalArea = otherOperationalArea;
    }

    public boolean isOperationalAreaOther() {
        return operationalAreaOther;
    }
}
