package org.smartregister.reveal.view;

import android.app.Activity;
import android.app.ProgressDialog;
import android.content.Intent;

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
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.shadows.ShadowApplication;
import org.robolectric.shadows.ShadowProgressDialog;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.presenter.OtherFormsPresenter;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.JSON_FORM_PARAM_JSON;
import static org.smartregister.reveal.util.Constants.RequestCode.REQUEST_CODE_GET_JSON;

/**
 * Created by Richard Kareko on 4/1/20.
 */

public class SummaryFormsActivityTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private OtherFormsPresenter presenter;

    @Mock
    private RevealJsonFormUtils jsonFormUtils;

    @Mock
    private Intent intent;

    @Captor
    private ArgumentCaptor<JSONObject> jsonObjectArgumentCaptor;

    @Captor
    private ArgumentCaptor<String> stringArgumentCaptor;

    private SummaryFormsActivity summaryFormsActivity;

    private android.content.Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        summaryFormsActivity = Robolectric.buildActivity(SummaryFormsActivity.class).create().resume().get();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(summaryFormsActivity);
        assertNotNull(Whitebox.getInternalState(summaryFormsActivity, "jsonFormUtils"));
        assertNotNull(Whitebox.getInternalState(summaryFormsActivity, "presenter"));
    }

    @Test
    public void testShowProgressBar() {
        summaryFormsActivity.showProgressDialog(R.string.saving_title);
        ProgressDialog progressDialog = (ProgressDialog) ShadowProgressDialog.getLatestDialog();
        assertNotNull(progressDialog);
        assertTrue(progressDialog.isShowing());
        assertEquals(context.getString(R.string.saving_title), ShadowApplication.getInstance().getLatestDialog().getTitle());
    }

    @Test
    public void testHideProgressdialog() {
        summaryFormsActivity.showProgressDialog(R.string.saving_title);
        ProgressDialog progressDialog = (ProgressDialog) ShadowProgressDialog.getLatestDialog();
        summaryFormsActivity.hideProgressDialog();
        assertNotNull(progressDialog);
        assertFalse(progressDialog.isShowing());
    }

    @Test
    public void testGetStartFormActivity() throws JSONException {
        Whitebox.setInternalState(summaryFormsActivity, "jsonFormUtils", jsonFormUtils);
        JSONObject jsonForm = new JSONObject(TestingUtils.structureJSON);
        summaryFormsActivity = spy(summaryFormsActivity);
        summaryFormsActivity.startFormActivity(jsonForm);

        verify(jsonFormUtils).startJsonForm(jsonObjectArgumentCaptor.capture(), any());
        assertNotNull(jsonObjectArgumentCaptor.getValue());
    }

    @Test
    public void testOnActivityResult() throws JSONException {
        Whitebox.setInternalState(summaryFormsActivity, "presenter", presenter);
        JSONObject jsonForm = new JSONObject(TestingUtils.structureJSON);
        when(intent.hasExtra(JSON_FORM_PARAM_JSON)).thenReturn(true);
        when(intent.getStringExtra(JSON_FORM_PARAM_JSON)).thenReturn(jsonForm.toString());

        summaryFormsActivity = spy(summaryFormsActivity);
        summaryFormsActivity.onActivityResult(REQUEST_CODE_GET_JSON, Activity.RESULT_OK, intent);

        verify(presenter).saveJsonForm(stringArgumentCaptor.capture());
        assertEquals(jsonForm.toString(), stringArgumentCaptor.getValue());
    }
}
