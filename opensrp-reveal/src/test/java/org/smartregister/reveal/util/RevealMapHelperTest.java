package org.smartregister.reveal.util;

import com.mapbox.mapboxsdk.style.layers.CircleLayer;
import com.mapbox.mapboxsdk.style.sources.GeoJsonSource;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.powermock.api.mockito.PowerMockito.whenNew;

/**
 * Created by Vincent Karuri on 08/05/2019
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest(RevealMapHelper.class)
public class RevealMapHelperTest {

    private RevealMapHelper revealMapHelper = null;

    @Before
    public void setUp() throws Exception {
        GeoJsonSource source = mock(GeoJsonSource.class);
        whenNew(GeoJsonSource.class).withAnyArguments().thenReturn(source);
        revealMapHelper = new RevealMapHelper();
    }

    @Test
    public void testGetIndexCaseCircleLayer() {
        CircleLayer circleLayer = mock(CircleLayer.class);
        Whitebox.setInternalState(revealMapHelper, "indexCaseCircleLayer", circleLayer);
        assertEquals(circleLayer, revealMapHelper.getIndexCaseCircleLayer());
    }
}
