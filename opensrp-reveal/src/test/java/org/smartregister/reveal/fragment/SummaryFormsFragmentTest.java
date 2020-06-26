package org.smartregister.reveal.fragment;

import android.app.AlertDialog;
import android.app.ProgressDialog;
import androidx.appcompat.app.AppCompatActivity;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.shadows.ShadowAlertDialog;
import org.robolectric.shadows.ShadowApplication;
import org.robolectric.shadows.ShadowProgressDialog;
import org.robolectric.shadows.ShadowToast;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.presenter.OtherFormsFragmentPresenter;

import java.util.ArrayList;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.smartregister.Context.bindtypes;

/**
 * Created by Richard Kareko on 3/2/20.
 */

public class SummaryFormsFragmentTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private OtherFormsFragmentPresenter presenter;

    private SummaryFormsFragment fragment;

    @Before
    public void setUp() {
        bindtypes = new ArrayList<>();
        fragment = new SummaryFormsFragment();
        AppCompatActivity activity = Robolectric.buildActivity(AppCompatActivity.class).create().start().get();
        activity.setContentView(R.layout.activity_summary_forms);
        activity.getSupportFragmentManager().beginTransaction().add(fragment, "Summary Forms").commit();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(Whitebox.getInternalState(fragment, "presenter"));
        assertNotNull(Whitebox.getInternalState(fragment, "progressDialog"));
        assertNotNull(Whitebox.getInternalState(fragment, "locationUtils"));
    }

    @Test
    public void testOnCreateview() {
        assertNotNull(Whitebox.getInternalState(fragment, "btnDailySummary"));
        assertNotNull(Whitebox.getInternalState(fragment, "btnTeamLeaderDos"));
        assertNotNull(Whitebox.getInternalState(fragment, "btnCbSprayArea"));
        assertNotNull(Whitebox.getInternalState(fragment, "btnIrsSaDecision"));
        assertNotNull(Whitebox.getInternalState(fragment, "btnMobilization"));
        assertNotNull(Whitebox.getInternalState(fragment, "btnIrsFieldOfficer"));
        assertNotNull(Whitebox.getInternalState(fragment, "btnVerificationForm"));

    }

    @Test
    public void testShowProgressDialog() {
        fragment.showProgressDialog(R.string.saving_title, R.string.saving_message);
        ProgressDialog progressDialog = (ProgressDialog) ShadowProgressDialog.getLatestAlertDialog();
        assertTrue(progressDialog.isShowing());
        assertEquals(getString(R.string.saving_title), ShadowApplication.getInstance().getLatestAlertDialog().getTitle());
        assertEquals(getString(R.string.saving_message), ShadowApplication.getInstance().getLatestAlertDialog().getMessage());
    }

    @Test
    public void testHideProgressDialog() {
        fragment.showProgressDialog(R.string.saving_title, R.string.saving_message);
        ProgressDialog progressDialog = (ProgressDialog) ShadowProgressDialog.getLatestAlertDialog();
        assertTrue(progressDialog.isShowing());

        fragment.hideProgressDialog();
        assertFalse(progressDialog.isShowing());
        assertEquals(getString(R.string.saving_title), ShadowApplication.getInstance().getLatestAlertDialog().getTitle());
        assertEquals(getString(R.string.saving_message), ShadowApplication.getInstance().getLatestAlertDialog().getMessage());
    }

    @Test
    public void testDisplayToast() {
        fragment.displayToast("Test");
        Toast toast = ShadowToast.getLatestToast();
        assertEquals(Toast.LENGTH_LONG, toast.getDuration());
        assertEquals("Test", ShadowToast.getTextOfLatestToast());
    }

    @Test
    public void testDisplayError() {
        fragment.displayError(R.string.form_save_failure_title, R.string.spray_form_save_failure);
        AlertDialog alertDialog = (AlertDialog) ShadowAlertDialog.getLatestDialog();
        assertTrue(alertDialog.isShowing());
        TextView tv = alertDialog.findViewById(android.R.id.message);
        assertEquals(getString(R.string.spray_form_save_failure), tv.getText());
    }

    @Test
    public void testOpeningDailySummaryForm() {
        Whitebox.setInternalState(fragment, "presenter", presenter);
        Button btnDailySummary = Whitebox.getInternalState(fragment, "btnDailySummary");
        doNothing().when(presenter).showBasicForm(anyString());
        btnDailySummary.performClick();
        verify(presenter).showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.DAILY_SUMMARY_ZAMBIA);
    }

    @Test
    public void testOpeningTeamLeaderDosForm() {
        Whitebox.setInternalState(fragment, "presenter", presenter);
        Button btnTeamLeaderDos = Whitebox.getInternalState(fragment, "btnTeamLeaderDos");
        doNothing().when(presenter).showBasicForm(anyString());
        btnTeamLeaderDos.performClick();
        verify(presenter).showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.TEAM_LEADER_DOS_ZAMBIA);
    }

    @Test
    public void testOpeningCBSprayForm() {
        Whitebox.setInternalState(fragment, "presenter", presenter);
        Button btnCbSprayArea = Whitebox.getInternalState(fragment, "btnCbSprayArea");
        doNothing().when(presenter).showBasicForm(anyString());
        btnCbSprayArea.performClick();
        verify(presenter).showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.CB_SPRAY_AREA_ZAMBIA);
    }

    @Test
    public void testOpeningIRSSADecisionForm() {
        Whitebox.setInternalState(fragment, "presenter", presenter);
        Button btnIrsSaDecision = Whitebox.getInternalState(fragment, "btnIrsSaDecision");
        doNothing().when(presenter).showBasicForm(anyString());
        btnIrsSaDecision.performClick();
        verify(presenter).showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.IRS_SA_DECISION_ZAMBIA);
    }

    @Test
    public void testOpeningMobilizationForm() {
        Whitebox.setInternalState(fragment, "presenter", presenter);
        Button btnMobilization = Whitebox.getInternalState(fragment, "btnMobilization");
        doNothing().when(presenter).showBasicForm(anyString());
        btnMobilization.performClick();
        verify(presenter).showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.MOBILIZATION_FORM_ZAMBIA);
    }

    @Test
    public void testOpeningIRSFieldOfficerForm() {
        Whitebox.setInternalState(fragment, "presenter", presenter);
        Button btnIrsFieldOfficer = Whitebox.getInternalState(fragment, "btnIrsFieldOfficer");
        doNothing().when(presenter).showBasicForm(anyString());
        btnIrsFieldOfficer.performClick();
        verify(presenter).showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.IRS_FIELD_OFFICER_ZAMBIA);
    }

    @Test
    public void testOpeningVerificationForm() {
        Whitebox.setInternalState(fragment, "presenter", presenter);
        Button btnVerificationForm = Whitebox.getInternalState(fragment, "btnVerificationForm");
        doNothing().when(presenter).showBasicForm(anyString());
        btnVerificationForm.performClick();
        verify(presenter).showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.VERIFICATION_FORM_ZAMBIA);
    }

}
