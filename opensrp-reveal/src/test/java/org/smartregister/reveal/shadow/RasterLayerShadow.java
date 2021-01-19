package org.smartregister.reveal.shadow;

import com.mapbox.mapboxsdk.style.layers.RasterLayer;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;

/**
 * Created by samuelgithengi on 1/12/21.
 */

@Implements(RasterLayer.class)
public class RasterLayerShadow extends LayerShadow {

    private String sourceId;

    @Implementation
    public void __constructor__(String layerId, String sourceId) {
        // Do nothing
        setLayerId(layerId);
        this.sourceId = sourceId;
    }

    @Implementation
    public String getSourceId() {
        return sourceId;
    }

}