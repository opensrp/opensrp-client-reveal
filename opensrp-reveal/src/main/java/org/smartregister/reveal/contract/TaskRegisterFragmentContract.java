package org.smartregister.reveal.contract;


import android.location.Location;
import android.support.annotation.StringRes;

import org.smartregister.reveal.model.TaskDetails;
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
    }

    interface View extends BaseRegisterFragmentContract.View {

        Location getLastLocation();

        void initializeAdapter(Set<org.smartregister.configurableviews.model.View> visibleColumns);

        void setTotalTasks(int structuresWithinBuffer);

        void setTaskDetails(List<TaskDetails> tasks);

        void displayNotification(int title, @StringRes int message, Object... formatArgs);
    }

}
