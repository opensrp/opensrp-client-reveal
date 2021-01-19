package org.smartregister.reveal.view;

import android.content.Intent;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.robolectric.Robolectric;
import org.robolectric.android.controller.ActivityController;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.Context;
import org.smartregister.family.contract.FamilyRegisterContract;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.service.ZiggyService;

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

    private ActivityController<FamilyRegisterActivity> activityController;

    @Before
    public void setUp() {
        ReflectionHelpers.setField(Context.getInstance(), "ziggyService", ziggyService);
        activityController = Robolectric.buildActivity(FamilyRegisterActivity.class);
    }

    @Test
    public void testOnCreateShouldNotStartRegistration() throws Exception {
        activity = Robolectric.buildActivity(FamilyRegisterActivity.class).get();
        activity = spy(activity);
        activity.onCreate(null);
        verify(activity, never()).startRegistration();
    }

    @Test
    public void testOnCreateShouldStartRegistration() throws Exception {
        Intent intent = new Intent();
        intent.putExtra("START_REGISTRATION", true);
        activity = Robolectric.buildActivity(FamilyRegisterActivity.class, intent).get();
        activity = spy(activity);
        activity.onCreate(null);
        verify(activity).startRegistration();

    }
}
