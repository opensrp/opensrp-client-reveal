package org.smartregister.reveal.fragment;

import android.app.ProgressDialog;
import android.support.v7.app.AppCompatActivity;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.presenter.OtherFormsFragmentPresenter;
import org.smartregister.reveal.util.LocationUtils;

import java.util.ArrayList;

import static org.junit.Assert.assertNotNull;
import static org.smartregister.Context.bindtypes;

/**
 * Created by Richard Kareko on 3/2/20.
 */

public class SummaryFormsFragmentTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    OtherFormsFragmentPresenter presenter;

    @Mock
    LocationUtils locationUtils;

    @Mock
    ProgressDialog progressDialog;

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

}
