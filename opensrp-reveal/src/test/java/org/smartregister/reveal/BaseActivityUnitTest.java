package org.smartregister.reveal;

import android.app.Activity;

import org.robolectric.android.controller.ActivityController;

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
