package org.smartregister.reveal.validators;

import com.mapbox.geojson.Feature;
import com.mapbox.mapboxsdk.camera.CameraPosition;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.util.TestingUtils;
import org.smartregister.reveal.view.RevealMapView;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 4/16/20.
 */

public class WithinOperationAreaValidatorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    RevealMapView revealMapView;

    @Mock
    Feature operationalArea;

    @Mock
    private MapboxMap mapboxMap;

    private WithinOperationAreaValidator validator;

    @Before
    public void setUp() {
        validator = new WithinOperationAreaValidator("error", revealMapView, operationalArea);
    }

    @Test
    public void testInitialization() {
        assertNotNull(Whitebox.getInternalState(validator, "mapView"));
        assertNotNull(Whitebox.getInternalState(validator, "operationalArea"));
    }

    @Test
    public void testIsValidWithPointInsideOA() {
        LatLng target = new LatLng(15.0665603, 101.1760481 );
        initializeMocks(target);

        boolean isvalid = validator.isValid("criteria", false);
        assertTrue(isvalid);

    }

    @Test
    public void testIsValidWithPointOutsideOA() {
        LatLng target = new LatLng(19.0665603, 90.1760481 );
        initializeMocks(target);

        boolean isvalid = validator.isValid("criteria", false);
        assertFalse(isvalid);

    }

    @Test
    public void testSetDisabled() {
        assertFalse(Whitebox.getInternalState(validator, "disabled"));
        validator.setDisabled(true);
        assertTrue(Whitebox.getInternalState(validator, "disabled"));
    }

    private void initializeMocks(LatLng target) {
        when(mapboxMap.getCameraPosition()).thenReturn(new CameraPosition.Builder().zoom(18).target(target).build());
        when(revealMapView.getMapboxMap()).thenReturn(mapboxMap);
        Feature feature = Feature.fromJson(TestingUtils.feature);
        when(operationalArea.geometry()).thenReturn(feature.geometry());
    }
}
