package org.smartregister.reveal.contract;

import android.content.Intent;
import androidx.annotation.StringRes;

import com.google.android.flexbox.FlexboxLayout;

import org.smartregister.reveal.model.TaskFilterParams;

import java.util.List;

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

        void setSortBySelection(int sortBySpinner);
    }

    interface Presenter {

        @StringRes
        Integer getStringResource(String intervention);

        void onToggleChanged(boolean isChecked, Object filterCategory, Object filterKey);

        List<String> getIntentionTypes();

        List<String> getBusinessStatusOptions();

        void onApplyFilters(String selectedItem);

        void restoreCheckedFilters(TaskFilterParams taskFilterParams);
    }
}
