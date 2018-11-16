package org.smartregister.reveal;

import android.app.Activity;

import org.robolectric.android.controller.ActivityController;

/**
 * Created by ndegwamartin on 24/07/2018.
 */
public abstract class BaseActivityUnitTest extends BaseUnitTest {

    protected void destroyController() {
        try {
            getActivity().finish();
            getActivityController().pause().stop().destroy(); //destroy controller if we can

        } catch (Exception e) {
            e.printStackTrace();
        }

        System.gc();
    }

    protected abstract Activity getActivity();

    protected abstract ActivityController getActivityController();


}
