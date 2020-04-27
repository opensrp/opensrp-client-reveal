package org.smartregister.reveal.shadow;

import android.content.Context;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import android.util.AttributeSet;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;

import io.ona.kujaku.helpers.MapboxLocationComponentWrapper;
import io.ona.kujaku.views.KujakuMapView;

/**
 * Created by samuelgithengi on 1/23/20.
 */
@Implements(KujakuMapView.class)
public class KujakuMapViewShadow extends MapViewShadow {

    @Implementation
    public void __constructor__(@NonNull Context context, @Nullable AttributeSet attrs) {
        // Do nothing
    }

    @Implementation
    public MapboxLocationComponentWrapper getMapboxLocationComponentWrapper() {
        return new MapboxLocationComponentWrapper();
    }

    @Implementation
    public void showCurrentLocationBtn(boolean isVisible) {
        //Do nothing
    }

    @Implementation
    public void onDestroy() {
        //Do nothing
    }

    @Implementation
    public void onResume(){//Do nothing
    }


}
