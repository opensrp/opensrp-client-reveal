package org.smartregister.reveal.shadow;

import android.content.Context;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import android.util.AttributeSet;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;
import org.smartregister.reveal.view.RevealMapView;

/**
 * Created by samuelgithengi on 1/23/20.
 */
@Implements(RevealMapView.class)
public class RevealMapViewShadow extends KujakuMapViewShadow {

    @Implementation
    public void __constructor__(@NonNull Context context, @Nullable AttributeSet attrs) {
        // Do nothing
    }

}
