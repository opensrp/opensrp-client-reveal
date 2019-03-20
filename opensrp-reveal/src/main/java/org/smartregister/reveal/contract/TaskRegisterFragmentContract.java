package org.smartregister.reveal.contract;


import android.location.Location;

import org.smartregister.configurableviews.model.View;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.view.contract.BaseRegisterFragmentContract;

import java.util.List;
import java.util.Set;

/**
 * Created by samuelgithengi on 3/18/19.
 */
public interface TaskRegisterFragmentContract {

    interface Presenter extends BaseRegisterFragmentContract.Presenter {
        void onTasksFound(List<TaskDetails> tasks);

        void onDestroy();
    }

    interface View extends BaseRegisterFragmentContract.View {

        Location getLastLocation();


        void initializeAdapter(Set<org.smartregister.configurableviews.model.View> visibleColumns);


        void setTaskDetails(List<TaskDetails> tasks);
    }

}
