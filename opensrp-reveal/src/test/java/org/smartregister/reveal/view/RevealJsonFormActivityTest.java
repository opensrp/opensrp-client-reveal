package org.smartregister.reveal.view;

import android.app.Activity;
import android.app.ProgressDialog;
import android.content.Intent;

import com.vijay.jsonwizard.constants.JsonFormConstants;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.shadows.ShadowApplication;
import org.robolectric.shadows.ShadowProgressDialog;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.activity.RevealJsonFormActivity;
import org.smartregister.reveal.fragment.RevealJsonFormFragment;
import org.smartregister.reveal.presenter.RevealJsonFormFragmentPresenter;
import org.smartregister.reveal.presenter.ValidateUserLocationPresenter;
import org.smartregister.reveal.util.LocationUtils;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;

import io.ona.kujaku.listeners.BaseLocationListener;
import io.ona.kujaku.utils.Constants;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 4/2/20.
 */

public class RevealJsonFormActivityTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private RevealJsonFormFragment formFragment;

    @Mock
    private RevealJsonFormFragmentPresenter presenter;

    @Mock
    private LocationUtils locationUtils;

    @Mock
    private BaseLocationListener locationListener;

    @Mock
    private ValidateUserLocationPresenter locationPresenter;

    private RevealJsonFormActivity revealJsonFormActivity;

    private android.content.Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        Intent intent = new Intent();
        intent.putExtra(JsonFormConstants.JSON_FORM_KEY.JSON, TestingUtils.DUMMY_JSON_FORM_STRING);
        revealJsonFormActivity = Robolectric.buildActivity(RevealJsonFormActivity.class, intent).create().resume().get();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(revealJsonFormActivity);
    }

    @Test
    public void testShowProgressDialog() {
        revealJsonFormActivity.showProgressDialog(R.string.saving_title, R.string.saving_message);
        ProgressDialog progressDialog = (ProgressDialog) ShadowProgressDialog.getLatestDialog();
        assertNotNull(progressDialog);
        assertTrue(progressDialog.isShowing());
        assertEquals(context.getString(R.string.saving_title), ShadowApplication.getInstance().getLatestDialog().getTitle());
    }

    @Test
    public void testHideProgressDialog() {
        revealJsonFormActivity.showProgressDialog(R.string.saving_title, R.string.saving_message);
        ProgressDialog progressDialog = (ProgressDialog) ShadowProgressDialog.getLatestDialog();
        revealJsonFormActivity.hideProgressDialog();
        assertNotNull(progressDialog);
        assertFalse(progressDialog.isShowing());
    }

    @Test
    public void testGetUserCurrentLocation() {
        revealJsonFormActivity = spy(revealJsonFormActivity);
        initializeMocks();

        revealJsonFormActivity.getUserCurrentLocation();

        verify(formFragment).getPresenter();
        verify(presenter).getLastLocation();
    }

    @Test
    public void testRequestUserLocation() {
        revealJsonFormActivity = spy(revealJsonFormActivity);
        initializeMocks();
        revealJsonFormActivity.requestUserLocation();

        verify(locationUtils).checkLocationSettingsAndStartLocationServices(revealJsonFormActivity, locationListener);
    }

    @Test
    public void testOnActivityResultForLocationSettings() throws Exception {
        Whitebox.setInternalState(revealJsonFormActivity, "formFragment",formFragment);
        Whitebox.setInternalState(revealJsonFormActivity, "requestedLocation", true);
        when(formFragment.getPresenter()).thenReturn(presenter);
        when(presenter.getLocationPresenter()).thenReturn(locationPresenter);
        when(presenter.getLocationUtils()).thenReturn(locationUtils);
        Whitebox.invokeMethod(revealJsonFormActivity, "onActivityResult",Constants.RequestCode.LOCATION_SETTINGS, Activity.RESULT_OK, null);
        verify(locationUtils).requestLocationUpdates(any());
        verify(locationPresenter).waitForUserLocation();
        assertFalse(Whitebox.getInternalState(revealJsonFormActivity, "requestedLocation"));
    }
    @Test
    public void testOnActivityResultForCancelledLocationSettings() throws Exception {
        Whitebox.setInternalState(revealJsonFormActivity, "formFragment",formFragment);
        Whitebox.setInternalState(revealJsonFormActivity, "requestedLocation", true);
        when(formFragment.getPresenter()).thenReturn(presenter);
        when(presenter.getLocationPresenter()).thenReturn(locationPresenter);
        when(presenter.getLocationUtils()).thenReturn(locationUtils);
        Whitebox.invokeMethod(revealJsonFormActivity, "onActivityResult",Constants.RequestCode.LOCATION_SETTINGS, Activity.RESULT_CANCELED, null);
        verify(locationPresenter).onGetUserLocationFailed();
        assertFalse(Whitebox.getInternalState(revealJsonFormActivity, "requestedLocation"));
    }


    private void initializeMocks() {
        Whitebox.setInternalState(revealJsonFormActivity, "formFragment", formFragment);
        when(formFragment.getPresenter()).thenReturn(presenter);
        when(presenter.getLocationUtils()).thenReturn(locationUtils);
        when(presenter.getLocationListener()).thenReturn(locationListener);
    }

}
