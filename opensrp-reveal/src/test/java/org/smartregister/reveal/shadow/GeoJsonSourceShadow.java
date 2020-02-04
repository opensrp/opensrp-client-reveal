package org.smartregister.reveal.shadow;

import com.mapbox.geojson.Feature;
import com.mapbox.mapboxsdk.style.sources.GeoJsonSource;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;

/**
 * Created by samuelgithengi on 2/4/20.
 */
@Implements(GeoJsonSource.class)
public class GeoJsonSourceShadow extends SourceShadow {

    public String id;
    @Implementation
    public void __constructor__(String id, Feature feature) {
        // Do nothing
    }
}
