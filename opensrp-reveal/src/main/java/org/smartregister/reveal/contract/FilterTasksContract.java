package org.smartregister.reveal.contract;

import android.content.Intent;
import android.support.annotation.StringRes;

import com.google.android.flexbox.FlexboxLayout;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by samuelgithengi on 12/18/19.
 */
public interface FilterTasksContract {

    interface View {

        void onFiltedSelected(int size);

        void applyFilters(Intent intent);

        FlexboxLayout getBusinessStatusLayout();

        FlexboxLayout getTaskCodeLayout();

        FlexboxLayout getInterventionTypeLayout();
    }

    interface Presenter {

        @StringRes
        Integer getStringResource(String intervention);

        void onToggleChanged(boolean isChecked, Object filterCategory, Object filterKey);

        List<String> getIntentionTypes();

        List<String> getBusinessStatusOptions();

        void onApplyFilters(String selectedItem);

        void restoreCheckedFilters(Map<String, Set<String>> checkedFilters);
    }
}
