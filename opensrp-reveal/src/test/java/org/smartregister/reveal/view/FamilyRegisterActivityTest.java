package org.smartregister.reveal.view;

import android.app.Activity;
import android.content.Intent;

import com.google.android.material.bottomnavigation.BottomNavigationView;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.robolectric.Robolectric;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.Context;
import org.smartregister.family.contract.FamilyRegisterContract;
import org.smartregister.family.util.Constants;
import org.smartregister.family.util.JsonFormUtils;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.service.ZiggyService;
import org.smartregister.view.fragment.BaseRegisterFragment;
import org.smartregister.view.viewpager.OpenSRPViewPager;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.robolectric.Shadows.shadowOf;

/**
 * Created by samuelgithengi on 1/19/21.
 */
public class FamilyRegisterActivityTest extends BaseUnitTest {

    private FamilyRegisterActivity activity;

    @Mock
    private ZiggyService ziggyService;

    @Mock
    private OpenSRPViewPager mPager;

    @Mock
    private FamilyRegisterContract.Presenter presenter;

    @Before
    public void setUp() {
        ReflectionHelpers.setField(Context.getInstance(), "ziggyService", ziggyService);
        activity = Robolectric.buildActivity(FamilyRegisterActivity.class).get();
    }

    @Test
    public void testOnCreateShouldNotStartRegistration() {
        activity = Robolectric.buildActivity(FamilyRegisterActivity.class).get();
        activity = spy(activity);
        activity.onCreate(null);
        verify(activity, never()).startRegistration();
    }

    @Test
    public void testOnCreateShouldStartRegistration() {
        Intent intent = new Intent();
        intent.putExtra("START_REGISTRATION", true);
        activity = Robolectric.buildActivity(FamilyRegisterActivity.class, intent).get();
        activity = spy(activity);
        activity.onCreate(null);
        verify(activity).startRegistration();

    }

    @Test
    public void testInitializePresenterShouldInitializePresenter() {
        assertNull(activity.presenter());
        activity.initializePresenter();
        assertNotNull(activity.presenter());

    }

    @Test
    public void testGetRegisterFragmentShouldReturnRegisterFragment() {
        MatcherAssert.assertThat(activity.getRegisterFragment(), Matchers.instanceOf(BaseRegisterFragment.class));
    }

    @Test
    public void testRegisterBottomNavigationShouldSetClientLabelAndRemoveOthers() {
        activity = Robolectric.buildActivity(FamilyRegisterActivity.class).create().start().get();
        BottomNavigationView bottomNavigationView = ReflectionHelpers.getField(activity, "bottomNavigationView");
        assertNull(bottomNavigationView.getMenu().findItem(R.id.action_clients));
        assertNull(bottomNavigationView.getMenu().findItem(R.id.action_search));
        assertNull(bottomNavigationView.getMenu().findItem(R.id.action_library));
        assertNull(bottomNavigationView.getMenu().findItem(R.id.action_job_aids));
        assertNull(bottomNavigationView.getMenu().findItem(R.id.action_register));
    }


    @Test
    public void testSwitchToFragmentShouldNavigateUp() {
        activity = spy(activity);
        ReflectionHelpers.setField(activity, "mPager", mPager);
        activity.switchToFragment(0);
        verify(activity).navigateUpTo(any());
        verifyZeroInteractions(mPager);
    }

    @Test
    public void testSwitchToFragmentShouldInvokeSuper() {
        activity = spy(activity);
        ReflectionHelpers.setField(activity, "mPager", mPager);
        activity.switchToFragment(2);
        verify(activity, never()).navigateUpTo(any());
        verify(mPager).setCurrentItem(2, false);
    }

    @Test
    public void testStartProfileActivityShouldStartProfileActivity() {
        activity.startProfileActivity("id1", "head1", "pg1", "doe");
        Intent startedIntent = shadowOf(activity).getNextStartedActivity();
        assertEquals(FamilyProfileActivity.class, shadowOf(startedIntent).getIntentClass());
        assertEquals("id1", startedIntent.getStringExtra(Constants.INTENT_KEY.FAMILY_BASE_ENTITY_ID));
        assertEquals("head1", startedIntent.getStringExtra(Constants.INTENT_KEY.FAMILY_HEAD));
        assertEquals("pg1", startedIntent.getStringExtra(Constants.INTENT_KEY.PRIMARY_CAREGIVER));
        assertEquals("doe", startedIntent.getStringExtra(Constants.INTENT_KEY.FAMILY_NAME));
        assertFalse(startedIntent.getBooleanExtra(Constants.INTENT_KEY.GO_TO_DUE_PAGE, true));

    }

    @Test
    public void testOnActivityResultShouldFinishActivity() {
        assertFalse(activity.isFinishing());
        activity.onActivityResult(JsonFormUtils.REQUEST_CODE_GET_JSON, Activity.RESULT_CANCELED, null);
        assertTrue(activity.isFinishing());
    }


    @Test
    public void testOnActivityResultShouldSaveForm() {
        ReflectionHelpers.setField(activity, "presenter", presenter);
        Intent data = new Intent();
        String json = "{\"encounter_type\":\"Family_Registration\"}";
        data.putExtra(Constants.JSON_FORM_EXTRA.JSON, json);
        activity.onActivityResult(JsonFormUtils.REQUEST_CODE_GET_JSON, Activity.RESULT_OK, data);
        verify(presenter).saveForm(json, false);
    }

    @Test
    public void testOnActivityResultShouldNotSaveFormWhenExceptionOccurs() {
        ReflectionHelpers.setField(activity, "presenter", presenter);
        Intent data = new Intent();
        String json = "{\"encounter_type\":\"Family_Registration\"";
        data.putExtra(Constants.JSON_FORM_EXTRA.JSON, json);
        activity.onActivityResult(JsonFormUtils.REQUEST_CODE_GET_JSON, Activity.RESULT_OK, data);
        verify(presenter,never()).saveForm(json, false);
        assertFalse(activity.isFinishing());
    }


}
