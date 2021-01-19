package org.smartregister.reveal.view;

import android.content.Intent;

import androidx.fragment.app.Fragment;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.robolectric.Robolectric;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.Context;
import org.smartregister.family.contract.FamilyRegisterContract;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.service.ZiggyService;
import org.smartregister.view.fragment.BaseRegisterFragment;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * Created by samuelgithengi on 1/19/21.
 */
public class FamilyRegisterActivityTest extends BaseUnitTest {

    private FamilyRegisterActivity activity;

    @Mock
    private FamilyRegisterContract.Presenter presenter;

    @Mock
    private ZiggyService ziggyService;

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
        presenter = activity.presenter();

    }

    @Test
    public void testGetRegisterFragmentShouldReturnRegisterFragment() {
        MatcherAssert.assertThat(activity.getRegisterFragment(), Matchers.instanceOf(BaseRegisterFragment.class));
    }

    @Test
    public void testGetOtherFragmentsReturnsEmptyArray() {
        assertArrayEquals(new Fragment[0], activity.getOtherFragments());
    }
}
