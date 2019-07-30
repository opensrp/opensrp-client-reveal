package org.smartregister.reveal.util;

import android.content.Context;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.mapboxsdk.camera.CameraPosition;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.Style;
import com.mapbox.mapboxsdk.style.layers.Layer;
import com.mapbox.mapboxsdk.style.layers.LineLayer;
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
import org.robolectric.RuntimeEnvironment;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyFloat;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.whenNew;
import static org.smartregister.reveal.util.RevealMapHelper.INDEX_CASE_LINE_LAYER;
import static org.smartregister.reveal.util.RevealMapHelper.INDEX_CASE_SYMBOL_LAYER;
import static org.smartregister.reveal.util.RevealMapHelper.MOSQUITO_COLLECTION_LAYER;
import static org.smartregister.reveal.util.RevealMapHelper.LARVAL_BREEDING_LAYER;
import static org.smartregister.reveal.util.RevealMapHelper.POTENTIAL_AREA_OF_TRANSMISSION_LAYER;


/**
 * Created by Vincent Karuri on 08/05/2019
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({RevealMapHelper.class, Utils.class, MapboxMap.class})
public class RevealMapHelperTest {

    private RevealMapHelper revealMapHelper = null;
    private String feature = "{\"geometry\":{\"coordinates\":[[[101.1761078,15.0666717],[101.1762902,15.0665732],[101.1762151,15.066467],[101.1760381,15.0665603],[101.1761078,15.0666717]]],\"type\":\"Polygon\"},\"id\":\"5d609281-6784-415e-9eee-8806c204b58c\",\"properties\":{\"is_index_case\":true,\"geographicLevel\":5.0,\"parentId\":\"450fc15b-5bd2-468a-927a-49cb10d3bcac\",\"taskStatus\":\"COMPLETED\",\"version\":0.0,\"taskCode\":\"Bednet Distribution\",\"status\":\"Active\",\"locationVersion\":\"0\",\"taskBusinessStatus\":\"Complete\",\"taskIdentifier\":\"c987a804-2525-43bd-99b1-e1910fffbc1a\"},\"type\":\"Feature\"}";

    @Captor
    private ArgumentCaptor<Feature> featureArgumentCaptor;

    @Captor
    private ArgumentCaptor<Layer> layerArgumentCaptor;

    private CameraPosition cameraPosition;

    private Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() throws Exception {
        GeoJsonSource source = mock(GeoJsonSource.class);
        whenNew(GeoJsonSource.class).withAnyArguments().thenReturn(source);
        SymbolLayer symbolLayer = mock(SymbolLayer.class);
        doReturn(INDEX_CASE_SYMBOL_LAYER).when(symbolLayer).getId();
        whenNew(SymbolLayer.class).withAnyArguments().thenReturn(symbolLayer);
        LineLayer lineLayer = mock(LineLayer.class);
        doReturn(INDEX_CASE_LINE_LAYER).when(lineLayer).getId();
        whenNew(LineLayer.class).withAnyArguments().thenReturn(lineLayer);
        mockStatic(Utils.class);
        PowerMockito.doReturn("12").when(Utils.class, "getGlobalConfig", any(), any());
        revealMapHelper = new RevealMapHelper();
    }

    @Test
    public void testGetIndexCase() {
        FeatureCollection featureCollection = FeatureCollection.fromFeature(Feature.fromJson(feature));
        assertNotNull(revealMapHelper.getIndexCase(featureCollection));
    }

    @Test
    public void testUpdateIndexCaseLayers() throws Exception {
        PowerMockito.mockStatic(Utils.class);
        GeoJsonSource source = mock(GeoJsonSource.class);
        Whitebox.setInternalState(revealMapHelper, "indexCaseSource", source);
        FeatureCollection featureCollection = FeatureCollection.fromFeature(Feature.fromJson(feature));
        MapboxMap mapboxMap = mock(MapboxMap.class);
        cameraPosition = new CameraPosition.Builder().target(new LatLng()).build();
        when(mapboxMap.getCameraPosition()).thenReturn(cameraPosition);
        when(Utils.createCircleFeature(any(), anyFloat(), anyFloat())).thenReturn(Feature.fromJson(feature));
        revealMapHelper.updateIndexCaseLayers(mapboxMap, featureCollection, context);
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
        GeoJsonSource source = mock(GeoJsonSource.class);
        doReturn(Feature.fromJson(feature)).when(revealMapHelper).getIndexCase(featureCollection);
        doReturn(style).when(mapboxMap).getStyle();
        doNothing().when(revealMapHelper).updateIndexCaseLayers(any(), any(), any());
        Whitebox.setInternalState(revealMapHelper, "indexCaseSource", source);
        revealMapHelper.addIndexCaseLayers(mapboxMap, context, featureCollection);
        verify(style, times(2)).addLayer(layerArgumentCaptor.capture());
        assertEquals(layerArgumentCaptor.getAllValues().get(0).getId(), INDEX_CASE_SYMBOL_LAYER);
        assertEquals(layerArgumentCaptor.getAllValues().get(1).getId(), INDEX_CASE_LINE_LAYER);
    }

    @Test
    public void testIndexCaseLayersNotAddedWhenIndexCaseIsNull() throws Exception {
        RevealMapHelper revealMapHelper = spy(this.revealMapHelper);
        MapboxMap mapboxMap = mock(MapboxMap.class);
        Context context = mock(Context.class);
        FeatureCollection featureCollection = mock(FeatureCollection.class);
        Style style = mock(Style.class);
        GeoJsonSource source = mock(GeoJsonSource.class);
        doReturn(null).when(revealMapHelper).getIndexCase(featureCollection);
        doReturn(style).when(mapboxMap).getStyle();
        Whitebox.setInternalState(revealMapHelper, "indexCaseSource", source);
        revealMapHelper.addIndexCaseLayers(mapboxMap, context, featureCollection);
        verify(style, times(0)).addLayer(layerArgumentCaptor.capture());
        assertNull(revealMapHelper.getIndexCaseLineLayer());
    }

    @Test
    public void testAddCustomLayers() throws Exception {

        RevealMapHelper revealMapHelper = spy(this.revealMapHelper);
        Context context = mock(Context.class);
        Style style = mock(Style.class);
        SymbolLayer symbolLayer = mock(SymbolLayer.class);
        whenNew(SymbolLayer.class).withAnyArguments()
                .thenReturn(symbolLayer);
        when(symbolLayer.getId())
                .thenReturn(MOSQUITO_COLLECTION_LAYER)
                .thenReturn(LARVAL_BREEDING_LAYER)
                .thenReturn(POTENTIAL_AREA_OF_TRANSMISSION_LAYER);
        revealMapHelper.addCustomLayers(style, context);
        verify(style, times(3)).addLayer(layerArgumentCaptor.capture());
        assertEquals(layerArgumentCaptor.getAllValues().get(0).getId(), MOSQUITO_COLLECTION_LAYER);
        assertEquals(layerArgumentCaptor.getAllValues().get(1).getId(), LARVAL_BREEDING_LAYER);
        assertEquals(layerArgumentCaptor.getAllValues().get(2).getId(), POTENTIAL_AREA_OF_TRANSMISSION_LAYER);

    }
}
