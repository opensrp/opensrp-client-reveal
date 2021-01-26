package org.smartregister.reveal.view;

import android.app.Activity;
import android.content.Intent;
import android.view.Menu;
import android.view.MenuItem;

import androidx.viewpager.widget.PagerAdapter;

import com.vijay.jsonwizard.activities.JsonWizardFormActivity;
import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.domain.Form;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.robolectric.Robolectric;
import org.robolectric.fakes.RoboMenu;
import org.robolectric.shadows.ShadowActivity;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.Context;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.family.contract.FamilyOtherMemberProfileFragmentContract;
import org.smartregister.family.util.Constants;
import org.smartregister.family.util.Constants.INTENT_KEY;
import org.smartregister.family.util.JsonFormUtils;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.fragment.FamilyOtherMemberProfileFragment;
import org.smartregister.reveal.presenter.FamilyOtherMemberPresenter;
import org.smartregister.view.activity.FormActivity;
import org.smartregister.view.viewpager.OpenSRPViewPager;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.robolectric.Shadows.shadowOf;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;

/**
 * Created by samuelgithengi on 1/26/21.
 */
public class FamilyOtherMemberProfileActivityTest extends BaseUnitTest {

    private FamilyOtherMemberProfileActivity activity;

    @Mock
    private OpenSRPViewPager mPager;

    @Mock
    private Menu menu;

    @Captor
    private ArgumentCaptor<PagerAdapter> pagerAdapterArgumentCaptor;

    @Mock
    private FamilyOtherMemberProfileContract.Presenter presenter;

    private Intent intent;

    @Mock
    private FamilyOtherMemberProfileFragmentContract.Presenter fragmentPresenter;

    @Mock
    private FamilyOtherMemberProfileFragment fragment;

    @Before
    public void setUp() {
        Context.bindtypes = new ArrayList<>();
        intent = new Intent();
        intent.putExtra(INTENT_KEY.BASE_ENTITY_ID, "1d1232");
        intent.putExtra(INTENT_KEY.FAMILY_BASE_ENTITY_ID, "f_1d1232");
        intent.putExtra(INTENT_KEY.FAMILY_HEAD, "doe");
        intent.putExtra(INTENT_KEY.PRIMARY_CAREGIVER, "PRIMARY_CAREGIVER");
        intent.putExtra(INTENT_KEY.FAMILY_NAME, "Dolce");
        intent.putExtra(STRUCTURE_ID, "s_122323");
        activity = Robolectric.buildActivity(FamilyOtherMemberProfileActivity.class, intent).get();

    }

    @Test
    public void testInitializePresenterShouldInitializePresenter() {
        assertNull(activity.presenter());
        activity.initializePresenter();
        assertNotNull(activity.presenter());
        assertFalse(ReflectionHelpers.getField(activity, "isFamilyHead"));
        FamilyOtherMemberPresenter presenter = (FamilyOtherMemberPresenter) activity.presenter();
        assertEquals("1d1232", presenter.getBaseEntityId());
        assertEquals("f_1d1232", ReflectionHelpers.getField(presenter, "familyBaseEntityId"));
        assertEquals("Dolce", ReflectionHelpers.getField(presenter, "familyName"));

    }

    @Test
    public void testSetupViewPagerShouldSetUpAdapter() {
        activity.setupViewPager(mPager);
        verify(mPager).setAdapter(pagerAdapterArgumentCaptor.capture());
        assertEquals(1, pagerAdapterArgumentCaptor.getValue().getCount());
        assertEquals("", pagerAdapterArgumentCaptor.getValue().getPageTitle(0));
    }

    @Test
    public void testOnCreateOptionsMenuShouldHideAddMember() {
        menu = new RoboMenu();
        menu.add(R.id.add_member);
        menu.add(R.id.action_archive);
        activity.onCreateOptionsMenu(menu);
        MenuItem item = menu.findItem(R.id.add_member);
        assertFalse(item.isVisible());

        MenuItem archive = menu.findItem(R.id.action_archive);
        assertTrue(archive.isVisible());
    }

    @Test
    public void testOnCreateOptionsMenuShouldHideArchiveForFamilyHead() {
        menu = new RoboMenu();
        menu.add(R.id.action_archive);
        ReflectionHelpers.setField(activity, "isFamilyHead", true);
        activity.onCreateOptionsMenu(menu);
        MenuItem item = menu.findItem(R.id.action_archive);
        assertFalse(item.isVisible());
    }

    @Test
    public void testOnClickUpShouldReturnHome() {
        activity = Robolectric.buildActivity(FamilyOtherMemberProfileActivity.class, intent).create().resume().get();
        ShadowActivity shadowActivity = shadowOf(activity);
        shadowActivity.clickMenuItem(android.R.id.home);

        assertTrue(activity.isFinishing());

    }


    @Test
    public void testOnClickRegistrationShouldEditRegistration() {
        ReflectionHelpers.setField(activity, "presenter", presenter);
        menu = new RoboMenu();
        activity.getMenuInflater().inflate(R.menu.menu_profile_activity, menu);
        ShadowActivity shadowActivity = shadowOf(activity);
        shadowActivity.clickMenuItem(R.id.action_registration);
        verify(presenter).onEditMemberDetails();

    }

    @Test
    public void testOnClickRegistrationShouldInvokeArchive() {
        ReflectionHelpers.setField(activity, "presenter", presenter);
        menu = new RoboMenu();
        activity.getMenuInflater().inflate(R.menu.menu_profile_activity, menu);
        ShadowActivity shadowActivity = shadowOf(activity);
        shadowActivity.clickMenuItem(R.id.action_archive);
        verify(presenter).onArchiveFamilyMember();

    }

    @Test
    public void testOnClicKOthersShouldInvokeParent() {
        menu = new RoboMenu();
        activity.getMenuInflater().inflate(R.menu.menu_profile_activity, menu);
        ShadowActivity shadowActivity = shadowOf(activity);
        shadowActivity.clickMenuItem(R.id.add_member);
        verifyNoMoreInteractions(presenter);
        Intent intentResult = shadowActivity.getNextStartedActivityForResult().intent;
        assertNotNull(intentResult);
        assertEquals(FormActivity.class, shadowOf(intentResult).getIntentClass());

    }

    @Test
    public void testStartFormActivityShouldStartForm() throws JSONException {
        String json = "{\"encounter_type\":\"Family_Member_Registration\"}";
        activity.startFormActivity(new JSONObject(json));
        ShadowActivity shadowActivity = shadowOf(activity);
        Intent intentResult = shadowActivity.getNextStartedActivityForResult().intent;
        assertNotNull(intentResult);
        assertEquals(JsonWizardFormActivity.class, shadowOf(intentResult).getIntentClass());
        assertEquals(json, intentResult.getStringExtra(Constants.JSON_FORM_EXTRA.JSON));
        Form form = (Form) intentResult.getSerializableExtra(JsonFormConstants.JSON_FORM_KEY.FORM);
        assertEquals(R.color.family_actionbar, form.getActionBarBackground());
        assertFalse(form.isWizard());

    }

    @Test
    public void testGetContextReturnsActivity() {
        assertEquals(activity, activity.getContext());
    }

    @Test
    public void testRefreshListShouldInvokeRefreshListView() {
        activity.setupViewPager(mPager);
        ViewPagerAdapter adapter = ReflectionHelpers.getField(activity, "adapter");

        adapter.addFragment(fragment, "familyOtherMemberProfileFragment");

        when(fragment.presenter()).thenReturn(fragmentPresenter);
        activity.refreshList();

        verify(fragment, timeout(ASYNC_TIMEOUT)).refreshListView();

    }

    @Test
    public void testOnActivityResultShouldSaveForm() {
        ReflectionHelpers.setField(activity, "presenter", presenter);
        Intent data = new Intent();
        String json = "{\"encounter_type\":\"Update_Family_Member_Registration\"}";
        data.putExtra(Constants.JSON_FORM_EXTRA.JSON, json);
        activity.onActivityResult(JsonFormUtils.REQUEST_CODE_GET_JSON, Activity.RESULT_OK, data);
        verify(presenter).updateFamilyMember(json);
    }

    @Test
    public void testOnActivityResultShouldNotSaveFormWhenExceptionOccurs() {
        ReflectionHelpers.setField(activity, "presenter", presenter);
        Intent data = new Intent();
        String json = "{\"encounter_type\":\"Update_Family_Member_Registration\"";
        data.putExtra(Constants.JSON_FORM_EXTRA.JSON, json);
        activity.onActivityResult(JsonFormUtils.REQUEST_CODE_GET_JSON, Activity.RESULT_OK, data);
        verify(presenter, never()).updateFamilyMember(json);
    }
}
