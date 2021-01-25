package org.smartregister.reveal.layer;

import com.mapbox.mapboxsdk.style.layers.Layer;
import com.mapbox.mapboxsdk.style.layers.RasterLayer;
import com.mapbox.mapboxsdk.style.sources.RasterSource;
import com.mapbox.mapboxsdk.style.sources.Source;
import com.mapbox.mapboxsdk.utils.ThreadUtils;

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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

/**
 * Created by samuelgithengi on 1/12/21.
 */
@Config(shadows = {RasterSourceShadow.class, RasterLayerShadow.class})
public class MapboxLayerTest extends BaseUnitTest {

    private MapBoxLayer layer;


    @Before
    public void setUp() {
        ThreadUtils.init(RuntimeEnvironment.application);
        layer = new MapBoxLayer();

    }

    @Test
    public void testConstructorShouldInitializesLayers() {
        LinkedHashSet<Layer> layers = ReflectionHelpers.getField(layer, "layers");
        List<Source> sources = ReflectionHelpers.getField(layer, "sources");
        assertEquals(1, layers.size());
        assertEquals(1, sources.size());
        RasterLayer rasterLayer = (RasterLayer) layers.iterator().next();
        assertEquals(MapBoxLayer.satelliteLayerId, rasterLayer.getId());
        assertEquals(MapBoxLayer.satelliteSourceId, rasterLayer.getSourceId());

        RasterSource rasterSource = (RasterSource) sources.get(0);
        assertEquals(MapBoxLayer.satelliteSourceId, rasterSource.getId());
    }

    @Test
    public void testGetDisplayNameShouldReturnCorrectName() {
        assertEquals("Mapbox Satellite", layer.getDisplayName());
    }


    @Test
    public void testGetSourceIdsReturnCorrectIds() {
        assertArrayEquals(new String[]{MapBoxLayer.satelliteSourceId}, layer.getSourceIds());
    }


    @Test
    public void testGetLayersReturnCorrectLayers() {
        LinkedHashSet<Layer> layers = layer.getLayers();
        assertEquals(1, layers.size());
        Layer layer = layers.iterator().next();
        MatcherAssert.assertThat(layers.iterator().next(), Matchers.instanceOf(RasterLayer.class));
        assertEquals(MapBoxLayer.satelliteLayerId, layer.getId());
        assertEquals(MapBoxLayer.satelliteSourceId, ((RasterLayer) layer).getSourceId());
    }

    @Test
    public void testGetSourcesReturnCorrectSources() {
        List<Source> sources = layer.getSources();
        assertEquals(1, sources.size());
        Source source = sources.iterator().next();
        MatcherAssert.assertThat(sources.iterator().next(), Matchers.instanceOf(Source.class));
        assertEquals(MapBoxLayer.satelliteSourceId, source.getId());
        RasterSource rasterSource = (RasterSource) source;
        assertEquals(MapBoxLayer.satelliteSourceId, rasterSource.getUri());
    }

    @Test
    public void testGetIdShouldReturnCorrectId() {
        assertEquals("mapbox-satellite-base-layer", layer.getId());
    }

    @Test
    public void testGetLayerIdsShouldReturnCorrectId() {
        assertArrayEquals(new String[]{MapBoxLayer.satelliteLayerId}, layer.getLayerIds());
    }

}
