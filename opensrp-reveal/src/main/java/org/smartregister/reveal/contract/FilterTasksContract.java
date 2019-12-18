package org.smartregister.reveal.contract;

import android.support.annotation.StringRes;

import java.util.List;

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

        void onToggleChanged(boolean isChecked, Object filterCategory, Object filterKey);

        List<String> getIntentionTypes();

        List<String> getBusinessStatusOptions();
    }
}
