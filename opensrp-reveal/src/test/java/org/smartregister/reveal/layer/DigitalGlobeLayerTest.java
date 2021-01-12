package org.smartregister.reveal.layer;

import com.mapbox.mapboxsdk.style.layers.Layer;
import com.mapbox.mapboxsdk.style.layers.RasterLayer;
import com.mapbox.mapboxsdk.style.sources.RasterSource;
import com.mapbox.mapboxsdk.style.sources.Source;
import com.mapbox.mapboxsdk.utils.ThreadUtils;

import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.annotation.Config;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.shadow.RasterLayerShadow;
import org.smartregister.reveal.shadow.RasterSourceShadow;

import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

/**
 * Created by samuelgithengi on 1/12/21.
 */
@Config(shadows = {RasterSourceShadow.class, RasterLayerShadow.class})
public class DigitalGlobeLayerTest extends BaseUnitTest {

    private DigitalGlobeLayer layer;


    @Before
    public void setUp() {
        ThreadUtils.init(RuntimeEnvironment.application);
        layer = new DigitalGlobeLayer();

    }

    @Test
    public void testConstructorInitializesLayers() {
        LinkedHashSet<Layer> layers = ReflectionHelpers.getField(layer, "layers");
        List<Source> sources = ReflectionHelpers.getField(layer, "sources");
        assertEquals(1, layers.size());
        assertEquals(1, sources.size());
        RasterLayer rasterLayer = (RasterLayer) layers.iterator().next();
        assertEquals("DG-EarthWatch-Satellite", rasterLayer.getId());
        assertEquals("dg-earthWatch-imagery", rasterLayer.getSourceId());

        RasterSource rasterSource = (RasterSource) sources.get(0);
        assertEquals("dg-earthWatch-imagery", rasterSource.getId());
    }

}
