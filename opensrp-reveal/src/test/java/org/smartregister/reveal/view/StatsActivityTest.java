package org.smartregister.reveal.view;

import android.app.ProgressDialog;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.shadows.ShadowApplication;
import org.robolectric.shadows.ShadowProgressDialog;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * Created by Richard Kareko on 4/1/20.
 */

public class StatsActivityTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private StatsActivity statsActivity;

    private android.content.Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        statsActivity = Robolectric.buildActivity(StatsActivity.class).create().resume().get();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(statsActivity);
    }

    @Test
    public void testShowProgressBar() {
        statsActivity.showProgressDialog(R.string.saving_title);
        ProgressDialog progressDialog = (ProgressDialog) ShadowProgressDialog.getLatestDialog();
        assertNotNull(progressDialog);
        assertTrue(progressDialog.isShowing());
        assertEquals(context.getString(R.string.saving_title), ShadowApplication.getInstance().getLatestDialog().getTitle());
    }

    @Test
    public void testHideProgressdialog() {
        statsActivity.showProgressDialog(R.string.saving_title);
        ProgressDialog progressDialog = (ProgressDialog) ShadowProgressDialog.getLatestDialog();
        statsActivity.hideProgressDialog();
        assertNotNull(progressDialog);
        assertFalse(progressDialog.isShowing());
    }
}
