package org.smartregister.reveal.activity;

import android.content.Intent;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.robolectric.Robolectric;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.android.controller.ActivityController;
import org.robolectric.annotation.Config;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.Context;
import org.smartregister.CoreLibrary;
import org.smartregister.reveal.TestRevealApplication;
import org.smartregister.reveal.contract.ChildProfileContract;
import org.smartregister.reveal.view.ChildProfileActivity;

import java.util.ArrayList;

/**
 * @author ronald
 */

@RunWith(RobolectricTestRunner.class)
@Config(application = TestRevealApplication.class, sdk = 22)
public class ChildProfileActivityTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private ChildProfileActivity activity;
    private String childBaseID = "childBaseID";
    private ActivityController<ChildProfileActivity> controller;

    @Mock
    private ChildProfileContract.Presenter presenter;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        controller = Robolectric.buildActivity(ChildProfileActivity.class).create().start();
        activity = controller.get();


        Context context = Context.getInstance();
        CoreLibrary.init(context);

        //Auto login by default
        String password = "pwd";
        context.session().start(context.session().lengthInMilliseconds());
        context.configuration().getDrishtiApplication().setPassword(password);
        context.session().setPassword(password);

        MockitoAnnotations.initMocks(this);
        Intent testIntent = new Intent();
        testIntent.putExtra(org.smartregister.reveal.util.Constants.Properties.BASE_ENTITY_ID, childBaseID);
        controller = Robolectric.buildActivity(ChildProfileActivity.class, testIntent).create().start();

        activity = controller.get();
        ReflectionHelpers.setField(activity, "presenter", presenter);
    }

    @After
    public void tearDown() {
        try {
            activity.finish();
            controller.pause().stop().destroy(); //destroy controller if we can
        } catch (Exception e) {
            e.printStackTrace();
        }

        //logout
        Context context = Context.getInstance();
        context.session().expire();

        System.gc();
    }

    @Test
    public void testViewsCreated() {
        Assert.assertNotNull(ReflectionHelpers.getField(activity, "tvNames"));
        Assert.assertNotNull(ReflectionHelpers.getField(activity, "tvNumber"));
        Assert.assertNotNull(ReflectionHelpers.getField(activity, "tvGender"));
        Assert.assertNotNull(ReflectionHelpers.getField(activity, "tvAge"));
        Assert.assertNotNull(ReflectionHelpers.getField(activity, "progressBar"));
    }
}
