package org.smartregister.reveal.shadow;

import com.mapbox.mapboxsdk.style.sources.RasterSource;
import com.mapbox.mapboxsdk.style.sources.TileSet;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;

/**
 * Created by samuelgithengi on 1/12/21.
 */
@Implements(RasterSource.class)
public class RasterSourceShadow extends SourceShadow {


    private String id;
    private String uri;

    @Implementation
    public void __constructor__(String id, TileSet tileSet, int tileSize) {
        this.id = id;
    }

    @Implementation
    public void __constructor__(String id, String uri, int tileSize) {
        this.id = id;
        this.uri = uri;
    }

    @Implementation
    protected String nativeGetId() {
        return id;
    }

    @Implementation
    protected String nativeGetUrl() {
        return uri;
    }


}
