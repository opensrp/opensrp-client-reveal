package org.smartregister.reveal.util;

import android.content.Context;
import android.content.res.Resources;
import android.util.DisplayMetrics;
import android.util.TypedValue;

import com.mapbox.mapboxsdk.Mapbox;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.Projection;

import org.junit.Assert;
import org.junit.Test;
import org.powermock.reflect.Whitebox;
import org.smartregister.reveal.BaseUnitTest;

import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class UtilsRoboElectricTest extends BaseUnitTest {

    @Test
    public void testCalculateZoomLevelRadius() {
        MapboxMap mapboxMap = mock(MapboxMap.class);
        Projection projection = mock(Projection.class);
        Context context = mock(Context.class);
        Resources resources = mock(Resources.class);
        DisplayMetrics displayMetrics = mock(DisplayMetrics.class);
        when(mapboxMap.getProjection()).thenReturn(projection);
        when(projection.getMetersPerPixelAtLatitude(anyDouble())).thenReturn(2.0);
        when(context.getResources()).thenReturn(resources);
        when(resources.getDisplayMetrics()).thenReturn(displayMetrics);
        Whitebox.setInternalState(displayMetrics,"density", 1.0f);

        Assert.assertEquals(Utils.calculateZoomLevelRadius(mapboxMap, 2, 2,  context), 1, 0.00001);
    }
}
