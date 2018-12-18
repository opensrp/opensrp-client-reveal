package org.smartregister.reveal.view;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.util.AttributeSet;

import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMapOptions;

import io.ona.kujaku.views.KujakuMapView;

import static org.smartregister.reveal.util.Constants.MY_LOCATION_ZOOM_LEVEL;

/**
 * Created by samuelgithengi on 12/13/18.
 */
public class RevealMapView extends KujakuMapView {
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
        super.centerMap(point, animateToNewTargetDuration, MY_LOCATION_ZOOM_LEVEL);
    }
}
