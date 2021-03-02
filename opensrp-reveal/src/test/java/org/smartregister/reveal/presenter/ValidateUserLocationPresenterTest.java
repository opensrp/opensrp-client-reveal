package org.smartregister.reveal.presenter;

import android.location.Location;

import com.mapbox.mapboxsdk.geometry.LatLng;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.powermock.reflect.Whitebox;
import org.robolectric.util.ReflectionHelpers;
import org.robolectric.util.ReflectionHelpers.ClassParameter;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.UserLocationContract;

import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.timeout;
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

    private Location location;

    @Before
    public void setUp() {
        presenter = new ValidateUserLocationPresenter(locationView, callback);
        location = new Location("test");
        location.setLatitude(12.1212);
        location.setLongitude(67.2232);
        Whitebox.setInternalState(BuildConfig.class, "SELECT_JURISDICTION", false);
    }

    @Test
    public void testRequestUserLocationShouldInvokeGetUserLocation() {
        presenter.requestUserLocation();
        verify(locationView).requestUserLocation();
    }

    @Test
    public void testOnGetUserLocationShouldRequestSupervisorPassword() {

        when(callback.getTargetCoordinates()).thenReturn(new LatLng(11.23232, 34.223222));

        presenter.onGetUserLocation(location);
        verify(callback).requestUserPassword();
        verify(callback).getTargetCoordinates();
        verify(locationView).hideProgressDialog();
    }

    @Test
    public void testOnGetUserLocationShouldInvokeLocationValidate() {

        when(callback.getTargetCoordinates()).thenReturn(new LatLng(location.getLatitude(), location.getLongitude()));

        presenter.onGetUserLocation(location);
        verify(locationView).hideProgressDialog();
        verify(callback).getTargetCoordinates();
        verify(callback).onLocationValidated();
    }

    @Test
    public void testOnGetUserLocationFailedShouldRequestForPasword() {
        presenter.onGetUserLocationFailed();
        verify(locationView).hideProgressDialog();
        verify(callback).requestUserPassword();
    }


    @Test
    public void testWaitForUserLocationShouldInvokeOnGetUserLocation() {
        presenter = spy(presenter);
        when(callback.getTargetCoordinates()).thenReturn(new LatLng(location.getLatitude(), location.getLongitude()));

        when(locationView.getUserCurrentLocation()).thenReturn(location);
        presenter.waitForUserLocation();

        verify(presenter, timeout(ASYNC_TIMEOUT)).onGetUserLocation(location);
        verify(locationView, timeout(ASYNC_TIMEOUT)).hideProgressDialog();
        verify(callback, timeout(ASYNC_TIMEOUT)).getTargetCoordinates();
        verify(callback, timeout(ASYNC_TIMEOUT)).onLocationValidated();
    }


    @Test
    public void testWaitForUserLocationInvokeOnGetUserLocationFailed() {
        presenter = spy(presenter);
        when(locationView.getUserCurrentLocation()).thenAnswer(invocation -> {
            Thread.sleep(3000);
            return null;
        });
        ReflectionHelpers.callInstanceMethod(presenter, "waitForUserLocation", ClassParameter.from(long.class, 61000));

        verify(presenter, timeout(ASYNC_TIMEOUT)).onGetUserLocationFailed();
        verify(locationView, timeout(ASYNC_TIMEOUT)).hideProgressDialog();
        verify(callback, timeout(ASYNC_TIMEOUT)).requestUserPassword();
    }


    @Test
    public void testWaitForUserLocationTryEvery2SecondsUntilTimeout() {
        AtomicInteger count = new AtomicInteger();
        when(locationView.getUserCurrentLocation()).thenAnswer(invocation -> {
            Thread.sleep(1000);
            count.getAndIncrement();
            return count.get() == 3 ? location : null;
        });
        when(callback.getTargetCoordinates()).thenReturn(new LatLng(location.getLatitude(), location.getLongitude()));


        presenter.waitForUserLocation();
        int ASYNC_TIMEOUT = 10 * 1000;
        verify(locationView, timeout(ASYNC_TIMEOUT).times(3)).getUserCurrentLocation();
        verify(locationView, timeout(ASYNC_TIMEOUT).times(2)).showProgressDialog(R.string.narrowing_location_title, R.string.narrowing_location_message);
        verify(locationView, timeout(ASYNC_TIMEOUT)).hideProgressDialog();
        verify(callback, timeout(ASYNC_TIMEOUT)).onLocationValidated();

    }
}
