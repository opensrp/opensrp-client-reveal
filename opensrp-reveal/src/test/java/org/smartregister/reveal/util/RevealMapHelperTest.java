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
import com.mapbox.mapboxsdk.utils.ThreadUtils;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.whenNew;
import static org.smartregister.reveal.util.Constants.GeoJSON.IS_INDEX_CASE;
import static org.smartregister.reveal.util.RevealMapHelper.INDEX_CASE_LINE_LAYER;
import static org.smartregister.reveal.util.RevealMapHelper.INDEX_CASE_SYMBOL_LAYER;
import static org.smartregister.reveal.util.RevealMapHelper.LARVAL_BREEDING_LAYER;
import static org.smartregister.reveal.util.RevealMapHelper.MOSQUITO_COLLECTION_LAYER;
import static org.smartregister.reveal.util.RevealMapHelper.POTENTIAL_AREA_OF_TRANSMISSION_LAYER;


/**
 * Created by Vincent Karuri on 08/05/2019
 */

public class RevealMapHelperTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private GeoJsonSource geoJsonSource;

    @Mock
    private SymbolLayer symbolLayer;

    @Mock
    private LineLayer lineLayer;

    @Mock
    private MapboxMap mapboxMap;

    @Mock
    private Style style;

    private RevealMapHelper revealMapHelper;
    private String feature = "{\"geometry\":{\"coordinates\":[[[101.1761078,15.0666717],[101.1762902,15.0665732],[101.1762151,15.066467],[101.1760381,15.0665603],[101.1761078,15.0666717]]],\"type\":\"Polygon\"},\"id\":\"5d609281-6784-415e-9eee-8806c204b58c\",\"properties\":{\"is_index_case\":true,\"geographicLevel\":5.0,\"parentId\":\"450fc15b-5bd2-468a-927a-49cb10d3bcac\",\"taskStatus\":\"COMPLETED\",\"version\":0.0,\"taskCode\":\"Bednet Distribution\",\"status\":\"Active\",\"locationVersion\":\"0\",\"taskBusinessStatus\":\"Complete\",\"taskIdentifier\":\"c987a804-2525-43bd-99b1-e1910fffbc1a\"},\"type\":\"Feature\"}";

    @Captor
    private ArgumentCaptor<Feature> featureArgumentCaptor;

    @Captor
    private ArgumentCaptor<Layer> layerArgumentCaptor;

    private Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() throws Exception {
        doReturn(INDEX_CASE_SYMBOL_LAYER).when(symbolLayer).getId();
        whenNew(SymbolLayer.class).withAnyArguments().thenReturn(symbolLayer);
        doReturn(INDEX_CASE_LINE_LAYER).when(lineLayer).getId();
        whenNew(LineLayer.class).withAnyArguments().thenReturn(lineLayer);
        revealMapHelper = new RevealMapHelper();
        ThreadUtils.init(context);
    }

    @Test
    public void testGetIndexCase() {
        FeatureCollection featureCollection = FeatureCollection.fromFeature(Feature.fromJson(feature));
        assertNotNull(revealMapHelper.getIndexCase(featureCollection));
    }

    @Test
    public void testUpdateIndexCaseLayers() throws JSONException {
        Feature feature = Feature.fromJson(this.feature);
        Whitebox.setInternalState(revealMapHelper, "indexCaseSource", geoJsonSource);
        FeatureCollection featureCollection = FeatureCollection.fromFeature(feature);
        CameraPosition cameraPosition = new CameraPosition.Builder().target(new LatLng()).build();
        when(mapboxMap.getCameraPosition()).thenReturn(cameraPosition);
        revealMapHelper.updateIndexCaseLayers(mapboxMap, featureCollection, context);
        verify(geoJsonSource).setGeoJson(featureArgumentCaptor.capture());
        assertEquals("Polygon", featureArgumentCaptor.getValue().geometry().type());
        JSONObject geometry = new JSONObject(featureArgumentCaptor.getValue().geometry().toJson());
        assertEquals(1, geometry.getJSONArray("coordinates").length());
        assertEquals(120, geometry.getJSONArray("coordinates").getJSONArray(0).length());
    }

    @Test
    public void testAddIndexCaseLayers() {
        Feature feature = Feature.fromJson(this.feature);
        FeatureCollection featureCollection = FeatureCollection.fromFeature(feature);
        doReturn(style).when(mapboxMap).getStyle();
        Whitebox.setInternalState(revealMapHelper, "indexCaseSource", geoJsonSource);
        revealMapHelper.addIndexCaseLayers(mapboxMap, context, featureCollection);
        verify(style, times(2)).addLayer(layerArgumentCaptor.capture());
        assertEquals(INDEX_CASE_SYMBOL_LAYER, layerArgumentCaptor.getAllValues().get(0).getId());
        assertEquals(INDEX_CASE_LINE_LAYER, layerArgumentCaptor.getAllValues().get(1).getId());
    }

    @Test
    public void testIndexCaseLayersNotAddedWhenIndexCaseIsNull() {
        Feature feature = Feature.fromJson(this.feature);
        feature.removeProperty(IS_INDEX_CASE);
        FeatureCollection featureCollection = FeatureCollection.fromFeature(feature);
        doReturn(style).when(mapboxMap).getStyle();
        Whitebox.setInternalState(revealMapHelper, "indexCaseSource", geoJsonSource);
        revealMapHelper.addIndexCaseLayers(mapboxMap, context, featureCollection);
        verify(style, never()).addLayer(layerArgumentCaptor.capture());
        assertNull(revealMapHelper.getIndexCaseLineLayer());

    }

    @Test
    public void testAddCustomLayers() {
        RevealMapHelper.addCustomLayers(style, context);
        verify(style, times(3)).addLayer(layerArgumentCaptor.capture());
        assertEquals(layerArgumentCaptor.getAllValues().get(0).getId(), MOSQUITO_COLLECTION_LAYER);
        assertEquals(layerArgumentCaptor.getAllValues().get(1).getId(), LARVAL_BREEDING_LAYER);
        assertEquals(layerArgumentCaptor.getAllValues().get(2).getId(), POTENTIAL_AREA_OF_TRANSMISSION_LAYER);

    }
}
