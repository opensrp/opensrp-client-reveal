package org.smartregister.reveal.view;

import android.app.ProgressDialog;

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

    @Captor
    private ArgumentCaptor<JSONObject> jsonObjectArgumentCaptor;

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
}
