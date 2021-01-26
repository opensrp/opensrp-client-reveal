package org.smartregister.reveal.view;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;

import androidx.viewpager.widget.ViewPager;

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
import org.smartregister.domain.Task;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.family.util.Constants;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.fragment.FamilyProfileMemberFragment;
import org.smartregister.reveal.fragment.StructureTasksFragment;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.RequestCode.REQUEST_CODE_GET_JSON_FRAGMENT;

/**
 * Created by Richard Kareko on 1/26/21.
 */

public class FamilyProfileActivityTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private FamilyProfileActivity familyProfileActivity;

    @Mock
    private StructureTasksFragment structureTasksFragment;

    @Mock
    private FamilyProfileMemberFragment profileMemberFragment;

    @Captor
    private ArgumentCaptor<ViewPagerAdapter> viewPagerAdapterArgumentCaptor;

    @Captor
    private ArgumentCaptor<Intent> intentArgumentCaptor;

    @Captor
    private ArgumentCaptor<Integer> integerArgumentCaptor;

    @Mock
    private ViewPager viewPager;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        familyProfileActivity = Robolectric.buildActivity(FamilyProfileActivity.class).create().resume().get();
        Whitebox.setInternalState(familyProfileActivity, "structureTasksFragment", structureTasksFragment);
        Whitebox.setInternalState(familyProfileActivity, "profileMemberFragment", profileMemberFragment);
    }

    @Test
    public void testOnCreate() {
        assertNotNull(Robolectric.buildActivity(FamilyProfileActivity.class).create().get());
    }

    @Test
    public void testInitializePresenter() {
        assertNull(Whitebox.getInternalState(familyProfileActivity, "presenter"));
        familyProfileActivity.initializePresenter();
        assertNotNull(Whitebox.getInternalState(familyProfileActivity, "presenter"));
    }

    @Test
    public void testSetUpViewPager() {
        assertNull(viewPager.getAdapter());
        viewPager = familyProfileActivity.setupViewPager(viewPager);
        verify(viewPager).setAdapter(viewPagerAdapterArgumentCaptor.capture());
        assertNotNull(viewPagerAdapterArgumentCaptor.getValue());
        assertEquals(2, viewPagerAdapterArgumentCaptor.getValue().getCount());
        assertEquals("RESIDENTS", viewPagerAdapterArgumentCaptor.getValue().getPageTitle(0));
        assertEquals("TASKS (0)", viewPagerAdapterArgumentCaptor.getValue().getPageTitle(1));
    }

    @Test
    public void testSetStructureId() {
        String structureId = "structure-1";
        familyProfileActivity.setStructureId(structureId);
        verify(structureTasksFragment).setStructure(structureId);
        verify(profileMemberFragment).setStructure(structureId);
    }

    @Test
    public void testUpdateFamilyName() {
        String familyName = "John";
        when(profileMemberFragment.getArguments()).thenReturn(new Bundle());
        familyProfileActivity.updateFamilyName(familyName);
        verify(profileMemberFragment, times(2)).getArguments();
        assertEquals(familyName, profileMemberFragment.getArguments().get(Constants.INTENT_KEY.FAMILY_NAME));
    }

    @Test
    public void testReturnToMapView() {
        String structureId = "structure-1";
        String taskEntityId = "task-entity-id-1";
        Task expectedTask = TestingUtils.getTask(taskEntityId);

        familyProfileActivity = spy(familyProfileActivity);
        familyProfileActivity.returnToMapView(structureId, expectedTask);
        verify(familyProfileActivity).finish();
        verify(familyProfileActivity).setResult(integerArgumentCaptor.capture(), intentArgumentCaptor.capture());
        assertEquals(Activity.RESULT_OK, integerArgumentCaptor.getValue().intValue());
        assertEquals(structureId, intentArgumentCaptor.getValue().getStringExtra(org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID));
        Task actualTask = (Task) intentArgumentCaptor.getValue().getSerializableExtra(org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_ID);
        assertNotNull(actualTask);
        assertEquals(taskEntityId, actualTask.getForEntity());
        assertEquals(expectedTask.getIdentifier(), actualTask.getIdentifier());
    }

    @Test
    public void testOnActivityResultForGetJsonFragment() {
        Intent data = new Intent();
        familyProfileActivity.onActivityResult(REQUEST_CODE_GET_JSON_FRAGMENT, Activity.RESULT_OK, data);
        verify(structureTasksFragment).onActivityResult(REQUEST_CODE_GET_JSON_FRAGMENT, Activity.RESULT_OK, data);
    }

}
