package org.smartregister.reveal.view;

import org.junit.Before;
import org.junit.Test;
import org.robolectric.Robolectric;
import org.smartregister.Context;
import org.smartregister.reveal.BaseUnitTest;

import java.util.ArrayList;

import static org.junit.Assert.assertNotNull;

/**
 * Created by samuelgithengi on 1/23/20.
 */
public class ListTasksActivityTest extends BaseUnitTest {


    private ListTasksActivity listTasksActivity;

    @Before
    public void setUp() {
        Context.bindtypes = new ArrayList<>();
        listTasksActivity = Robolectric.buildActivity(ListTasksActivity.class).create().get();
    }


    @Test
    public void testOnCreate() {
        assertNotNull(Robolectric.buildActivity(ListTasksActivity.class).create().get());
    }
}
