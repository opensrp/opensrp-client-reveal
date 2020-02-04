package org.smartregister.reveal.shadow;

import com.mapbox.mapboxsdk.style.sources.Source;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;

/**
 * Created by samuelgithengi on 2/4/20.
 */
@Implements(Source.class)
public class SourceShadow {

    @Implementation
    protected String nativeGetId() {
        return "sourceId";
    }
}
