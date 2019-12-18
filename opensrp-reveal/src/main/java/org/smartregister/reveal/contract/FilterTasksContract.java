package org.smartregister.reveal.contract;

import android.content.Intent;
import android.support.annotation.StringRes;

import java.util.List;

/**
 * Created by samuelgithengi on 12/18/19.
 */
public interface FilterTasksContract {

    interface View {

        void onFiltedSelected(int size);

        void applyFilters(Intent intent);
    }

    interface Presenter {

        @StringRes
        Integer getStringResource(String intervention);

        void onToggleChanged(boolean isChecked, Object filterCategory, Object filterKey);

        List<String> getIntentionTypes();

        List<String> getBusinessStatusOptions();

        void onApplyFilters(String selectedItem);
    }
}
