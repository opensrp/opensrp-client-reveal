package org.smartregister.reveal.presenter;

import android.location.Location;

import com.mapbox.mapboxsdk.geometry.LatLng;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.UserLocationContract;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 12/1/20.
 */
public class ValidateUserLocationPresenterTest extends BaseUnitTest {

    @Mock
    private UserLocationContract.UserLocationView locationView;

    @Mock
    private UserLocationContract.UserLocationCallback callback;

    private ValidateUserLocationPresenter presenter;

    @Before
    public void setUp() {
        presenter = new ValidateUserLocationPresenter(locationView, callback);
    }

    @Test
    public void testRequestUserLocationShouldInvokeGetUserLocation() {
        presenter.requestUserLocation();
        verify(locationView).requestUserLocation();
    }

    @Test
    public void testOnGetUserLocationShouldRequestSupervisorPassword() {

        Location location = new Location("test");
        location.setLatitude(12.1212);
        location.setLongitude(67.2232);
        when(callback.getTargetCoordinates()).thenReturn(new LatLng(11.23232, 34.223222));

        presenter.onGetUserLocation(location);
        verify(callback).requestUserPassword();
        verify(callback).getTargetCoordinates();
        verify(locationView).hideProgressDialog();
    }

    @Test
    public void testOnGetUserLocationShouldInvokeLocationValidate() {

        Location location = new Location("test");
        location.setLatitude(12.1212);
        location.setLongitude(67.2232);
        when(callback.getTargetCoordinates()).thenReturn(new LatLng(location.getLatitude(), location.getLongitude()));

        presenter.onGetUserLocation(location);
        verify(locationView).hideProgressDialog();
        verify(callback).getTargetCoordinates();
        verify(callback).onLocationValidated();
    }
}
