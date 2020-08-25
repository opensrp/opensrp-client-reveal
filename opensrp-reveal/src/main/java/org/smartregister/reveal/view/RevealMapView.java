package org.smartregister.reveal.view;

import android.content.Context;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import android.util.AttributeSet;

import com.mapbox.mapboxsdk.camera.CameraPosition;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.MapboxMapOptions;
import com.rengwuxian.materialedittext.validation.METValidator;

import java.util.ArrayList;
import java.util.List;

import io.ona.kujaku.views.KujakuMapView;

import static org.smartregister.reveal.util.Constants.MY_LOCATION_ZOOM_LEVEL;

/**
 * Created by samuelgithengi on 12/13/18.
 */
public class RevealMapView extends KujakuMapView {

    private List<METValidator> validators;

    private MapboxMap mapboxMap;

    public RevealMapView(@NonNull Context context) {
        super(context);
    }

    public RevealMapView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public RevealMapView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public RevealMapView(@NonNull Context context, @Nullable MapboxMapOptions options) {
        super(context, options);
    }

    @Override
    public void centerMap(@NonNull LatLng point, int animateToNewTargetDuration, double newZoom) {
        super.centerMap(point, animateToNewTargetDuration, newZoom > MY_LOCATION_ZOOM_LEVEL ? newZoom : MY_LOCATION_ZOOM_LEVEL);
    }


    public void addValidator(METValidator validator) {
        if (validators == null) {
            this.validators = new ArrayList<>();
        }
        this.validators.add(validator);
    }

    public List<METValidator> getValidators() {
        return validators;
    }

    public MapboxMap getMapboxMap() {
        return mapboxMap;
    }

    public void setMapboxMap(MapboxMap mapboxMap) {
        this.mapboxMap = mapboxMap;
    }


    public Double getMapboxMapZoom() {
        if (mapboxMap != null)
            return mapboxMap.getCameraPosition().zoom;
        else
            return null;
    }


    public CameraPosition getCameraPosition() {
        if (mapboxMap != null)
            return mapboxMap.getCameraPosition();
        else
            return null;
    }
}
