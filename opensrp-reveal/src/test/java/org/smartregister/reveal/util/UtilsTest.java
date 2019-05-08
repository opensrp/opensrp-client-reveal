package org.smartregister.reveal.util;

import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.Projection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.smartregister.reveal.R;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Utils.calculateZoomLevelRadius;
import static org.smartregister.reveal.util.Utils.getInterventionLabel;

/**
 * Created by Vincent Karuri on 08/05/2019
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest(PreferencesUtil.class)
public class UtilsTest {

    @Test
    public void testCalculateZoomLevelRadius() {
        MapboxMap mapboxMap = mock(MapboxMap.class);
        Projection projection = mock(Projection.class);
        doReturn(projection).when(mapboxMap).getProjection();
        doReturn(2.0).when(projection).getMetersPerPixelAtLatitude(anyDouble());
        assertEquals(calculateZoomLevelRadius(mapboxMap, 2, 2), 1, 0.00001);
    }

    @Test
    public void testGetInterventionLabel() throws Exception {
        PowerMockito.mockStatic(PreferencesUtil.class);
        PreferencesUtil preferencesUtil = mock(PreferencesUtil.class);
        PowerMockito.when(PreferencesUtil.class, "getInstance").thenReturn(preferencesUtil);
        when(preferencesUtil.getCurrentCampaignId()).thenReturn("IRS_1");
        assertEquals(getInterventionLabel(), R.string.irs);
    }
}
