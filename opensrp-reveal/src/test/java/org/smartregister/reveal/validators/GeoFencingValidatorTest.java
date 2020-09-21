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
import org.smartregister.reveal.R;
import org.smartregister.reveal.util.TestingUtils;
import org.smartregister.reveal.view.RevealMapView;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_NAME;

/**
 * Created by Richard Kareko on 4/16/20.
 */

public class GeoFencingValidatorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private RevealMapView revealMapView;

    @Mock
    private Feature operationalArea;

    @Mock
    private MapboxMap mapboxMap;

    private GeoFencingValidator validator;

    @Before
    public void setUp() {
        validator = new GeoFencingValidator("error", revealMapView, operationalArea);
    }

    @Test
    public void testInitialization() {
        assertNotNull(Whitebox.getInternalState(validator, "mapView"));
        assertNotNull(Whitebox.getInternalState(validator, "operationalArea"));
    }

    @Test
    public void testIsValidWithPointInsideOA() {
        LatLng target = new LatLng(15.0665603, 101.1760481);
        initializeMocks(target);

        boolean isvalid = validator.isValid("criteria", false);
        assertTrue(isvalid);

    }

    @Test
    public void testIsValidWithPointOutsideOA() {
        LatLng target = new LatLng(19.0665603, 90.1760481);
        initializeMocks(target);

        boolean isvalid = validator.isValid("criteria", false);
        assertFalse(isvalid);

    }

    @Test
    public void testIsValidWithPointOutsideOAWhenOnOtherOA() {
        when(operationalArea.hasProperty(LOCATION_NAME)).thenReturn(true);
        String name = "287 Palms Other";
        when(operationalArea.getStringProperty(LOCATION_NAME)).thenReturn(name);
        validator = new GeoFencingValidator("error", revealMapView, operationalArea);
        LatLng target = new LatLng(19.0665603, 90.1760481);
        initializeMocks(target);

        boolean isValid = validator.isValid("criteria", false);
        assertFalse(isValid);
        assertEquals(R.string.point_not_within_other_operational_area, validator.getErrorId());
        assertEquals(name, validator.getSelectedOperationalArea());
        assertArrayEquals(new String[]{name}, validator.getErrorMessageArgs());

    }

    @Test
    public void testIsValidWithPointInAnotherOperationalArea() {

        Feature validOA = Feature.fromJson(TestingUtils.operationalArea2Feature);
        validator.getOperationalAreas().add(validOA);
        LatLng target = new LatLng(-9.342380199986167, 28.857339199944192);
        initializeMocks(target);

        boolean isValid = validator.isValid("criteria", false);
        assertFalse(isValid);
        assertEquals(R.string.point_within_known_operational_area, validator.getErrorId());
        assertEquals(validOA.getStringProperty(LOCATION_NAME), validator.getSelectedOperationalArea());
        assertArrayEquals(new String[]{validOA.getStringProperty(LOCATION_NAME)}, validator.getErrorMessageArgs());

    }


    @Test
    public void testIsValidWithPointOutsideOAWhenOtherOAIsDefined() {

        Feature otherOA = Feature.fromJson(TestingUtils.operationalArea2Feature);
        validator.setOtherOperationalArea(otherOA);
        LatLng target = new LatLng(-9.342380199986167, 28.857339199944192);
        initializeMocks(target);

        boolean isValid = validator.isValid("criteria", false);
        assertFalse(isValid);
        assertEquals(R.string.point_not_within_known_operational_area, validator.getErrorId());
        assertEquals(otherOA.getStringProperty(LOCATION_NAME), validator.getSelectedOperationalArea());
        assertArrayEquals(new String[]{otherOA.getStringProperty(LOCATION_NAME)}, validator.getErrorMessageArgs());

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
