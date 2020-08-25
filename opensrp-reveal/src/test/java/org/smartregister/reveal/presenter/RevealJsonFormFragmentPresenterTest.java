package org.smartregister.reveal.presenter;

import android.content.Context;
import android.content.Intent;
import androidx.appcompat.app.AlertDialog;
import android.widget.ImageButton;

import com.mapbox.mapboxsdk.camera.CameraPosition;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.rengwuxian.materialedittext.validation.METValidator;
import com.vijay.jsonwizard.views.JsonFormFragmentView;

import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.activity.RevealJsonFormActivity;
import org.smartregister.reveal.fragment.RevealJsonFormFragment;
import org.smartregister.reveal.util.Constants.JsonForm;
import org.smartregister.reveal.validators.MinZoomValidator;
import org.smartregister.reveal.validators.WithinOperationAreaValidator;
import org.smartregister.reveal.view.RevealMapView;
import org.smartregister.util.AssetHandler;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 3/13/19.
 */
public class RevealJsonFormFragmentPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private RevealJsonFormFragmentPresenter presenter;

    private Context context = RuntimeEnvironment.application;

    private RevealJsonFormFragment formFragment;


    private RevealJsonFormActivity jsonFormActivity;

    @Mock
    private RevealMapView mapView;

    @Mock
    private WithinOperationAreaValidator withinOperationAreaValidator;

    @Mock
    private ImageButton imageButton;


    private void setUpFormActivity(String formName) {
        Intent intent = new Intent();
        intent.putExtra("json", AssetHandler.readFileFromAssetsFolder(formName, context));
        jsonFormActivity = Robolectric.buildActivity(RevealJsonFormActivity.class, intent).create().resume().get();
        formFragment = RevealJsonFormFragment.getFormFragment("step1");
        jsonFormActivity.getSupportFragmentManager().beginTransaction().add(formFragment, null).commit();
        presenter = formFragment.getPresenter();
        when(imageButton.getDrawable()).thenReturn(context.getDrawable(R.drawable.ic_cross_hair_blue));
        when(mapView.findViewById(R.id.ib_mapview_focusOnMyLocationIcon)).thenReturn(imageButton);
        CameraPosition cameraPosition = new CameraPosition.Builder().zoom(20).target(new LatLng()).build();
        when(mapView.getCameraPosition()).thenReturn(cameraPosition);

    }


    @Test
    public void testValidateWithInvalidFieldsShouldShowError() {
        setUpFormActivity(JsonForm.SPRAY_FORM);
        presenter = spy(presenter);
        JsonFormFragmentView view = spy(Whitebox.getInternalState(presenter, JsonFormFragmentView.class));
        Whitebox.setInternalState(presenter, "viewRef", new WeakReference<>(view));
        doNothing().when(view).showSnackBar(anyString());
        presenter.onSaveClick(formFragment.getMainView());
        verify(presenter).validateAndWriteValues();
        verify(view).showSnackBar(anyString());

    }


    @Test
    public void testValidateWithValidInputsShouldReturnToParentActivity() {
        setUpFormActivity(JsonForm.ADD_STRUCTURE_FORM);
        presenter = spy(presenter);
        JsonFormFragmentView view = spy(Whitebox.getInternalState(presenter, JsonFormFragmentView.class));
        Whitebox.setInternalState(presenter, "viewRef", new WeakReference<>(view));
        presenter.onSaveClick(formFragment.getMainView());
        verify(presenter).validateAndWriteValues();
        verify(view).finishWithResult(any());

    }


    @Test
    public void testValidateWithInvalidMapShouldShowError() {
        setUpFormActivity(JsonForm.ADD_STRUCTURE_FORM);
        presenter = spy(presenter);
        when(mapView.getValidators()).thenReturn(Collections.singletonList(new MinZoomValidator("error", 20)));
        when(mapView.getMapboxMapZoom()).thenReturn(15.5);
        formFragment.getJsonApi().addFormDataView(mapView);
        JsonFormFragmentView view = spy(Whitebox.getInternalState(presenter, JsonFormFragmentView.class));
        Whitebox.setInternalState(presenter, "viewRef", new WeakReference<>(view));

        doNothing().when(view).showSnackBar(anyString());
        presenter.onSaveClick(formFragment.getMainView());

        verify(presenter).validateAndWriteValues();
        verify(view).showSnackBar(anyString());

    }


    @Test
    public void testValidateWithMapShouldValidateLocation() {
        setUpFormActivity(JsonForm.ADD_STRUCTURE_FORM);
        presenter = spy(presenter);
        when(mapView.getValidators()).thenReturn(Collections.singletonList(new MinZoomValidator("error", 20)));
        when(mapView.getMapboxMapZoom()).thenReturn(20.5);
        formFragment.getJsonApi().addFormDataView(mapView);
        JsonFormFragmentView view = spy(Whitebox.getInternalState(presenter, JsonFormFragmentView.class));
        Whitebox.setInternalState(presenter, "viewRef", new WeakReference<>(view));
        jsonFormActivity = spy(jsonFormActivity);
        doReturn(null).when(jsonFormActivity).getUserCurrentLocation();


        Whitebox.setInternalState(presenter, "jsonFormView", jsonFormActivity);

        ValidateUserLocationPresenter userLocationPresenter = Whitebox.getInternalState(presenter, "locationPresenter");
        userLocationPresenter = spy(userLocationPresenter);
        doNothing().when(userLocationPresenter).requestUserLocation();

        Whitebox.setInternalState(presenter, "locationPresenter", userLocationPresenter);

        presenter.onSaveClick(formFragment.getMainView());

        verify(presenter).validateAndWriteValues();
        verify(jsonFormActivity).getUserCurrentLocation();


        verify(userLocationPresenter).requestUserLocation();

    }

    @Test
    public void testValidateWithMapShouldNotValidateLocationIfValidationIsDisabled() {
        setUpFormActivity(JsonForm.ADD_STRUCTURE_FORM);
        presenter = spy(presenter);
        when(mapView.getValidators()).thenReturn(Collections.singletonList(new MinZoomValidator("error", 20)));
        when(mapView.getMapboxMapZoom()).thenReturn(20.5);
        formFragment.getJsonApi().addFormDataView(mapView);
        JsonFormFragmentView view = spy(Whitebox.getInternalState(presenter, JsonFormFragmentView.class));
        Whitebox.setInternalState(presenter, "viewRef", new WeakReference<>(view));
        doReturn(false).when(presenter).validateFarStructures();
        presenter.onSaveClick(formFragment.getMainView());

        verify(presenter).validateAndWriteValues();
        verify(view).finishWithResult(any());

    }


    @Test
    public void testOnPasswordVerified() {
        setUpFormActivity(JsonForm.ADD_STRUCTURE_FORM);
        presenter = spy(presenter);
        JsonFormFragmentView view = spy(Whitebox.getInternalState(presenter, JsonFormFragmentView.class));
        Whitebox.setInternalState(presenter, "viewRef", new WeakReference<>(view));
        presenter.onPasswordVerified();
        verify(presenter).onLocationValidated();
    }

    @Test
    public void testRequestUserPassword() {
        setUpFormActivity(JsonForm.ADD_STRUCTURE_FORM);

        AlertDialog passwordDialog = Whitebox.getInternalState(presenter, "passwordDialog");
        passwordDialog = spy(passwordDialog);
        doNothing().when(passwordDialog).show();
        Whitebox.setInternalState(presenter, "passwordDialog", passwordDialog);
        presenter.requestUserPassword();

        verify(passwordDialog).show();

    }

    @Test
    public void testGetLocationPresenter() {
        setUpFormActivity(JsonForm.ADD_STRUCTURE_FORM);
        assertNotNull(presenter.getLocationPresenter());
    }


    @Test
    public void testValidateWithMapShouldValidateLocationIfValidateWithinOperationalAreaReturnsTrue() {
        setUpFormActivity(JsonForm.ADD_STRUCTURE_FORM);
        presenter = spy(presenter);
        List<METValidator> validators = new ArrayList<>();
        validators.add(new MinZoomValidator("error", 20));
        validators.add(withinOperationAreaValidator);
        when(mapView.getValidators()).thenReturn(validators);
        when(mapView.getMapboxMapZoom()).thenReturn(20.5);
        when(withinOperationAreaValidator.isValid(anyString(), anyBoolean())).thenReturn(true);
        formFragment.getJsonApi().addFormDataView(mapView);
        JsonFormFragmentView view = spy(Whitebox.getInternalState(presenter, JsonFormFragmentView.class));
        Whitebox.setInternalState(presenter, "viewRef", new WeakReference<>(view));
        jsonFormActivity = spy(jsonFormActivity);
        doReturn(null).when(jsonFormActivity).getUserCurrentLocation();


        Whitebox.setInternalState(presenter, "jsonFormView", jsonFormActivity);

        ValidateUserLocationPresenter userLocationPresenter = Whitebox.getInternalState(presenter, "locationPresenter");
        userLocationPresenter = spy(userLocationPresenter);
        doNothing().when(userLocationPresenter).requestUserLocation();

        Whitebox.setInternalState(presenter, "locationPresenter", userLocationPresenter);

        presenter.onSaveClick(formFragment.getMainView());

        verify(presenter).validateAndWriteValues();
        verify(jsonFormActivity).getUserCurrentLocation();


        verify(userLocationPresenter).requestUserLocation();


    }

}
