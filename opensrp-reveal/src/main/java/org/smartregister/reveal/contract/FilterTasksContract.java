package org.smartregister.reveal.contract;

import android.support.annotation.StringRes;

/**
 * Created by samuelgithengi on 12/18/19.
 */
public interface FilterTasksContract {

    interface View {

    }

    interface Presenter {

        @StringRes
        Integer getStringResource(String intervention);
    }
}
