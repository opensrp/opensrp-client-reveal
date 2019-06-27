package org.smartregister.reveal.presenter;

import android.content.Context;
import android.support.v7.app.AlertDialog;

import com.mapbox.mapboxsdk.geometry.LatLng;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.domain.Location;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.BaseFormFragmentContract;
import org.smartregister.reveal.interactor.BaseFormFragmentInteractor;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.repository.RevealMappingHelper;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 4/18/19.
 */
public class FormFragmentPresenterTest extends BaseUnitTest {


    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private BaseFormFragmentContract.View view;

    @Mock
    private RevealJsonFormUtils jsonFormUtils;

    @Mock
    private RevealMappingHelper mappingHelper;

    @Mock
    private AlertDialog passwordDialog;

    @Mock
    private BaseFormFragmentInteractor interactor;

    @Captor
    private ArgumentCaptor<JSONObject> jsonArgumentCaptor;

    private Context context = RuntimeEnvironment.application;

    private BaseFormFragmentPresenter presenter;

    private StructureTaskDetails taskDetails = TestingUtils.getStructureTaskDetails();

    @Mock
    private Location structure;


    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        presenter = new BaseFormFragmentPresenter(view, context);
        presenter.mappingHelper = mappingHelper;
        Whitebox.setInternalState(presenter, "taskDetails", taskDetails);
        Whitebox.setInternalState(presenter, "structure", structure);
        Whitebox.setInternalState(presenter, "passwordDialog", passwordDialog);
        Whitebox.setInternalState(presenter, "interactor", interactor);
        when(view.getJsonFormUtils()).thenReturn(jsonFormUtils);

    }

    @Test
    public void testOnPasswordVerifiedStartsForm() {
        taskDetails.setTaskCode(Constants.Intervention.BLOOD_SCREENING);
        when(jsonFormUtils.getFormName(null, taskDetails.getTaskCode())).thenReturn(Constants.JsonForm.BLOOD_SCREENING_FORM);
        presenter.onPasswordVerified();
        verify(view).startForm(null);

    }

    @Test
    public void testGetTargetCoordinates() {
        android.location.Location location = new android.location.Location("test");
        when(mappingHelper.getCenter(anyString())).thenReturn(location);
        LatLng target = presenter.getTargetCoordinates();
        assertEquals(location.getLatitude(), target.getLatitude(), 0.00001);
        assertEquals(location.getLongitude(), target.getLongitude(), 0.00001);

    }

    @Test
    public void testRequestUserPassword() {
        presenter.requestUserPassword();
        verify(passwordDialog).show();
    }


    @Test
    public void testOnOpenBednetFormPopulatesNumberOfMembers() {
        taskDetails.setTaskCode(Constants.Intervention.BEDNET_DISTRIBUTION);
        when(jsonFormUtils.getFormName(null, taskDetails.getTaskCode())).thenReturn(Constants.JsonForm.BEDNET_DISTRIBUTION_FORM);
        presenter.onLocationValidated();
        verify(view, never()).startForm(null);
        verify(interactor).findNumberOfMembers(taskDetails.getTaskEntity(), null);
    }


    @Test
    public void testOnFoundMembersCount() throws JSONException {
        presenter.onFetchedMembersCount(3, new JSONObject("{\"members\": \"[num_fam_members]\"}"));
        verify(view).startForm(jsonArgumentCaptor.capture());
        verify(view).hideProgressDialog();
        assertEquals(3, jsonArgumentCaptor.getValue().getInt("members"));
    }

    @Test
    public void testGetLocationPresenter() {
        assertNotNull(presenter.getLocationPresenter());
    }

}
