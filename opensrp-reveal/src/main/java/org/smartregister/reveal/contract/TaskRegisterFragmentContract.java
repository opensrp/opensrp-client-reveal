package org.smartregister.reveal.contract;


import android.location.Location;
import android.support.annotation.StringRes;

import org.json.JSONObject;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.view.contract.BaseRegisterFragmentContract;

import java.util.List;
import java.util.Set;

/**
 * Created by samuelgithengi on 3/18/19.
 */
public interface TaskRegisterFragmentContract {

    interface Presenter extends BaseRegisterFragmentContract.Presenter {
        void onTasksFound(List<TaskDetails> tasks, int structuresWithinBuffer);

        void onDestroy();

        void onDrawerClosed();

        void onTaskSelected(TaskDetails details);

        void onStructureFound(org.smartregister.domain.Location structure, TaskDetails details);
    }

    interface View extends BaseRegisterFragmentContract.View {

        Location getLastLocation();

        void initializeAdapter(Set<org.smartregister.configurableviews.model.View> visibleColumns);

        void setTotalTasks(int structuresWithinBuffer);

        void setTaskDetails(List<TaskDetails> tasks);

        void displayNotification(int title, @StringRes int message, Object... formatArgs);

        void startForm(JSONObject formName);

        RevealJsonFormUtils getJsonFormUtils();

        void showProgressDialog(@StringRes int title, @StringRes int message);

        void hideProgressDialog();
    }

}
