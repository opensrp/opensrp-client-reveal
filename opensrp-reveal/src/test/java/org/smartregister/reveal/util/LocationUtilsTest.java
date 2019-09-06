package org.smartregister.reveal.util;

import android.location.Location;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;

import io.ona.kujaku.interfaces.ILocationClient;
import io.ona.kujaku.listeners.BaseLocationListener;
import io.ona.kujaku.utils.LocationSettingsHelper;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 5/22/19.
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({LocationSettingsHelper.class})
public class LocationUtilsTest {

    private LocationUtils locationUtils;

    @Mock
    private ILocationClient locationClient;

    @Before
    public void setUp() {
        PowerMockito.mockStatic(LocationSettingsHelper.class);
        locationUtils = new LocationUtils(locationClient);
    }

    @Test
    public void testRequestLocationUpdates() {
        BaseLocationListener locationListener = new BaseLocationListener();
        locationUtils.requestLocationUpdates(locationListener);
        verify(locationClient).requestLocationUpdates(locationListener);
    }

    @Test
    public void testGetLastLocation() {
        Location location = new Location("LocationUtilsTest");
        when(locationClient.getLastLocation()).thenReturn(location);
        assertEquals(location, locationUtils.getLastLocation());
        verify(locationClient).getLastLocation();
    }

    @Test
    public void testStopLocationClient() {
        locationUtils.stopLocationClient();
        verify(locationClient).close();
        assertNotNull(Whitebox.getInternalState(locationUtils, "locationClient"));
    }

    @Test
    public void testDestroyLocationClient() {
        locationUtils.destroy();
        verify(locationClient).close();
        assertNull(Whitebox.getInternalState(locationUtils, "locationClient"));
    }

}
