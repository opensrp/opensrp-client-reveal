package org.smartregister.reveal.shadow;

import androidx.annotation.NonNull;

import com.mapbox.mapboxsdk.style.layers.Layer;
import com.mapbox.mapboxsdk.style.layers.PropertyValue;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;

/**
 * Created by samuelgithengi on 2/4/20.
 */
@Implements(Layer.class)
public class LayerShadow {

    private String layerId;

    @Implementation
    public void setProperties(@NonNull PropertyValue<?>... properties) {
        // Do nothing
    }

    @Implementation
    public String getId() {
        return layerId;
    }

    public void setLayerId(String layerId) {
        this.layerId = layerId;
    }
}
