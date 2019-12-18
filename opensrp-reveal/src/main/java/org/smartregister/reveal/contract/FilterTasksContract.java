package org.smartregister.reveal.contract;

import android.content.Context;
import android.support.annotation.StringRes;

/**
 * Created by samuelgithengi on 12/18/19.
 */
public interface FilterTasksContract {

    interface View {

        void onFiltedSelected(int size);
    }

    interface Presenter {

        @StringRes
        Integer getStringResource(String intervention);

        void onToggleChanged(int buttonId, Object filterCategory, Object filterKey);
    }
}
