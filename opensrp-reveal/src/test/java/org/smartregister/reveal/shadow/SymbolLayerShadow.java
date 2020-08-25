package org.smartregister.reveal.shadow;

import com.mapbox.mapboxsdk.style.layers.SymbolLayer;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;

/**
 * Created by samuelgithengi on 2/4/20.
 */
@Implements(SymbolLayer.class)
public class SymbolLayerShadow extends LayerShadow {

    @Implementation
    public void __constructor__(String layerId, String sourceId) {
        setLayerId(layerId);
    }

}
