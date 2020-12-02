package org.smartregister.reveal.fragment;

import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.Intent;
import android.view.View;
import android.widget.TextView;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.annotation.Config;
import org.robolectric.shadows.ShadowAlertDialog;
import org.robolectric.shadows.ShadowApplication;
import org.robolectric.shadows.ShadowProgressDialog;
import org.smartregister.cursoradapter.RecyclerViewPaginatedAdapter;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.activity.RevealJsonFormActivity;
import org.smartregister.reveal.model.EventRegisterDetails;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.presenter.EventRegisterFragmentPresenter;
import org.smartregister.reveal.shadow.DrawerMenuViewShadow;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.view.EventRegisterActivity;
import org.smartregister.reveal.view.FilterTasksActivity;

import java.util.ArrayList;
import java.util.HashSet;

import static android.app.Activity.RESULT_OK;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.robolectric.Shadows.shadowOf;
import static org.smartregister.reveal.util.Constants.BLOOD_SCREENING_EVENT;
import static org.smartregister.reveal.util.Constants.Filter.FILTER_SORT_PARAMS;
import static org.smartregister.reveal.util.Constants.JSON_FORM_PARAM_JSON;
import static org.smartregister.reveal.util.Constants.RequestCode.REQUEST_CODE_FILTER_TASKS;

/**
 * Created by Richard Kareko on 12/1/20.
 */

@Config(shadows = {DrawerMenuViewShadow.class})
public class EventRegisterFragmentTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private EventRegisterFragment fragment;

    private EventRegisterActivity activity;

    @Mock
    private RevealJsonFormUtils jsonFormUtils;

    @Mock
    private EventRegisterFragmentPresenter presenter;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        fragment = new EventRegisterFragment();
        Whitebox.setInternalState(fragment, "presenter", presenter);
        activity = Robolectric.buildActivity(EventRegisterActivity.class).create().start().get();
        activity.setContentView(R.layout.activity_base_register);
        activity.getSupportFragmentManager().beginTransaction().add(0, fragment).commit();

    }

    @Test
    public void testOnCreate() {
        assertNotNull(fragment);
    }

    @Test
    public void testGetLayout() {
        assertEquals(R.layout.fragment_event_register, fragment.getLayout());
    }

    @Test
    public void testInitializePresenter() {
        Whitebox.setInternalState(fragment, "presenter", (Object[]) null);
        assertNull(Whitebox.getInternalState(fragment, "presenter"));
        fragment.initializePresenter();
        assertNotNull(Whitebox.getInternalState(fragment, "presenter"));
    }

    @Test
    public void testInitializeAdapter() {
        fragment.initializeAdapter(new HashSet<>());
        RecyclerViewPaginatedAdapter clientAdapter = Whitebox.getInternalState(fragment, "clientAdapter");
        assertNotNull(clientAdapter);
        assertEquals(20, clientAdapter.getCurrentlimit());
    }

    @Test
    public void testStartMapActivity() {
        fragment.startMapActivity();
        assertTrue(activity.isFinishing());
    }

    @Test
    public void testViewClicked() {
        View view = fragment.getView();
        EventRegisterDetails details = new EventRegisterDetails();
        details.setEventType(BLOOD_SCREENING_EVENT);
        details.setFormSubmissionId("form-submission-id-1");
        view.setTag(R.id.patient_column, details);
        fragment.onViewClicked(view);
        verify(presenter).onEventSelected(details);
    }

    @Test
    public void testShowProgressDialog() {
        fragment.showProgressDialog(R.string.saving_title, R.string.saving_message);
        ProgressDialog progressDialog = (ProgressDialog) ShadowProgressDialog.getLatestDialog();
        assertTrue(progressDialog.isShowing());
        assertEquals(activity.getString(R.string.saving_title), ShadowApplication.getInstance().getLatestDialog().getTitle());
    }

    @Test
    public void testHideProgressDialog() {
        fragment.showProgressDialog(R.string.saving_title, R.string.saving_message);
        ProgressDialog progressDialog = (ProgressDialog) ShadowProgressDialog.getLatestDialog();
        assertTrue(progressDialog.isShowing());
        fragment.hideProgressDialog();
        assertFalse(progressDialog.isShowing());

    }

    @Test
    public void testOpenFilterActivity() {
        fragment.openFilterActivity(null);
        Intent intent = shadowOf(activity).getNextStartedActivity();
        assertEquals(FilterTasksActivity.class, shadowOf(intent).getIntentClass());
    }

    @Test
    public void testOnActivityResultFilterTasks() {
        Intent intent = new Intent();
        TaskFilterParams params = TaskFilterParams.builder().build();
        intent.putExtra(FILTER_SORT_PARAMS, params);
        fragment.onActivityResult(REQUEST_CODE_FILTER_TASKS, RESULT_OK, intent);
        verify(presenter).filterTasks(params);
    }

    @Test
    public void testGetJsonFormUtils() {
        assertNull(fragment.getJsonFormUtils());
        fragment.setJsonFormUtils(jsonFormUtils);
        assertEquals(jsonFormUtils, fragment.getJsonFormUtils());
    }

    @Test
    public void testStartForm() {
        JSONObject form = new JSONObject();
        fragment.startForm(form);
        Intent intent = shadowOf(activity).getNextStartedActivity();
        assertEquals(RevealJsonFormActivity.class, shadowOf(intent).getIntentClass());
        assertEquals(form.toString(), intent.getStringExtra(JSON_FORM_PARAM_JSON));
    }

    @Test
    public void testDisplayError() {
        fragment.displayError(R.string.opening_form_title, R.string.error_unable_to_start_form);
        AlertDialog dialog = (AlertDialog) ShadowAlertDialog.getLatestDialog();
        assertTrue(dialog.isShowing());
        assertEquals(activity.getString(R.string.error_unable_to_start_form), ((TextView) dialog.findViewById(android.R.id.message)).getText());
    }


}
