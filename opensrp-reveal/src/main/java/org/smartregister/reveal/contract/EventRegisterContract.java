package org.smartregister.reveal.contract;

import androidx.annotation.StringRes;

import org.json.JSONObject;
import org.smartregister.domain.Event;
import org.smartregister.reveal.model.EventRegisterDetails;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.view.contract.BaseRegisterFragmentContract;

import java.util.Set;

/**
 * Created by samuelgithengi on 7/30/20.
 */
public interface EventRegisterContract {
    interface View extends BaseRegisterFragmentContract.View {

        void initializeQueryParams(String tableName, String countSelect, String mainSelect);

        void initializeAdapter(Set<org.smartregister.configurableviews.model.View> visibleColumns);

        void countExecute();

        void filterandSortInInitializeQueries();

        void startMapActivity();

        RevealJsonFormUtils getJsonFormUtils();

        void startForm(JSONObject formName);

        void displayError(int title, int message);

        void showProgressDialog(@StringRes int title, @StringRes int message);

        void hideProgressDialog();

        void openFilterActivity(TaskFilterParams filterParams);
    }

    interface Presenter extends BaseRegisterFragmentContract.Presenter {
        void onEventFound(Event event);

        void onOpenMapClicked();

        void onEventSelected(EventRegisterDetails details);

        void onFilterTasksClicked();

        void filterTasks(TaskFilterParams filterParams);

        String getMainCondition();

        String getSortQuery();
    }

    interface Interactor {
        void findEvent(String formSubmissionId);
    }
}
