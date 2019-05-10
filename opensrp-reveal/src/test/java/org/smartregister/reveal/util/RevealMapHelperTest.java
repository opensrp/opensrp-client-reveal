package org.smartregister.reveal.util;

import android.content.Context;
import android.location.Location;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.Style;
import com.mapbox.mapboxsdk.style.layers.CircleLayer;
import com.mapbox.mapboxsdk.style.layers.Layer;
import com.mapbox.mapboxsdk.style.layers.PropertyValue;
import com.mapbox.mapboxsdk.style.layers.SymbolLayer;
import com.mapbox.mapboxsdk.style.sources.GeoJsonSource;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.ArgumentMatchers.anyFloat;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.whenNew;
import static org.smartregister.reveal.util.RevealMapHelper.INDEX_CASE_CIRCLE_LAYER;
import static org.smartregister.reveal.util.RevealMapHelper.INDEX_CASE_SYMBOL_LAYER;

/**
 * Created by Vincent Karuri on 08/05/2019
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({RevealMapHelper.class, Utils.class})
public class RevealMapHelperTest {

    private RevealMapHelper revealMapHelper = null;
    private String feature = "{\"geometry\":{\"coordinates\":[[[101.1761078,15.0666717],[101.1762902,15.0665732],[101.1762151,15.066467],[101.1760381,15.0665603],[101.1761078,15.0666717]]],\"type\":\"Polygon\"},\"id\":\"5d609281-6784-415e-9eee-8806c204b58c\",\"properties\":{\"is_index_case\":true,\"geographicLevel\":5.0,\"parentId\":\"450fc15b-5bd2-468a-927a-49cb10d3bcac\",\"taskStatus\":\"COMPLETED\",\"version\":0.0,\"taskCode\":\"Bednet Distribution\",\"status\":\"Active\",\"locationVersion\":\"0\",\"taskBusinessStatus\":\"Complete\",\"taskIdentifier\":\"c987a804-2525-43bd-99b1-e1910fffbc1a\"},\"type\":\"Feature\"}";

    @Captor
    private ArgumentCaptor<Feature> featureArgumentCaptor;

    @Captor
    private ArgumentCaptor<Layer> layerArgumentCaptor;

    @Before
    public void setUp() throws Exception {
        GeoJsonSource source = mock(GeoJsonSource.class);
        whenNew(GeoJsonSource.class).withAnyArguments().thenReturn(source);
        SymbolLayer symbolLayer = mock(SymbolLayer.class);
        doReturn(INDEX_CASE_SYMBOL_LAYER).when(symbolLayer).getId();
        whenNew(SymbolLayer.class).withAnyArguments().thenReturn(symbolLayer);
        CircleLayer circleLayer = mock(CircleLayer.class);
        doReturn(INDEX_CASE_CIRCLE_LAYER).when(circleLayer).getId();
        whenNew(CircleLayer.class).withAnyArguments().thenReturn(circleLayer);
        revealMapHelper = new RevealMapHelper();
    }

    @Test
    public void testGetIndexCaseCircleLayer() {
        CircleLayer circleLayer = mock(CircleLayer.class);
        Whitebox.setInternalState(revealMapHelper, "indexCaseCircleLayer", circleLayer);
        assertEquals(circleLayer, revealMapHelper.getIndexCaseCircleLayer());
    }

    @Test
    public void testResizeIndexCaseCircle() throws Exception {
        mockStatic(Utils.class);
        MapboxMap mapboxMap = mock(MapboxMap.class);
        CircleLayer circleLayer = mock(CircleLayer.class);
        Whitebox.setInternalState(revealMapHelper, "indexCaseCircleLayer", circleLayer);
        Whitebox.setInternalState(revealMapHelper, "indexCaseLocation", mock(Location.class));
        PowerMockito.doReturn("12").when(Utils.class, "getGlobalConfig", any(), any());
        PowerMockito.doReturn(12f).when(Utils.class, "calculateZoomLevelRadius", any(MapboxMap.class), anyDouble(), anyFloat());
        revealMapHelper.resizeIndexCaseCircle(mapboxMap);
        verify(circleLayer).setProperties(any(PropertyValue.class));
    }

    @Test
    public void testGetIndexCase() {
        FeatureCollection featureCollection = FeatureCollection.fromFeature(Feature.fromJson(feature));
        assertNotNull(revealMapHelper.getIndexCase(featureCollection));
    }

    @Test
    public void testUpdateIndexCaseLayers() throws Exception {
        mockStatic(Utils.class);
        GeoJsonSource source = mock(GeoJsonSource.class);
        Whitebox.setInternalState(revealMapHelper, "indexCaseSource", source);
        Whitebox.setInternalState(revealMapHelper, "indexCaseCircleLayer", mock(CircleLayer.class));
        FeatureCollection featureCollection = FeatureCollection.fromFeature(Feature.fromJson(feature));
        MapboxMap mapboxMap = mock(MapboxMap.class);
        PowerMockito.doReturn("12").when(Utils.class, "getGlobalConfig", any(), any());
        PowerMockito.doReturn(12f).when(Utils.class, "calculateZoomLevelRadius", any(MapboxMap.class), anyDouble(), anyFloat());
        revealMapHelper.updateIndexCaseLayers(mapboxMap, featureCollection);
        verify(source).setGeoJson(featureArgumentCaptor.capture());
        assertEquals(featureArgumentCaptor.getValue().getStringProperty("taskIdentifier"), "c987a804-2525-43bd-99b1-e1910fffbc1a");
    }

    @Test
    public void testAddIndexCaseLayers() throws Exception {
        RevealMapHelper revealMapHelper = spy(this.revealMapHelper);
        MapboxMap mapboxMap = mock(MapboxMap.class);
        Context context = mock(Context.class);
        FeatureCollection featureCollection = mock(FeatureCollection.class);
        Style style = mock(Style.class);
        doReturn(style).when(mapboxMap).getStyle();
        doNothing().when(revealMapHelper).updateIndexCaseLayers(any(), any());
        revealMapHelper.addIndexCaseLayers(mapboxMap, context, featureCollection);
        verify(style, times(2)).addLayer(layerArgumentCaptor.capture());
        assertEquals(layerArgumentCaptor.getAllValues().get(0).getId(), INDEX_CASE_SYMBOL_LAYER);
        assertEquals(layerArgumentCaptor.getAllValues().get(1).getId(), INDEX_CASE_CIRCLE_LAYER);
    }
}
