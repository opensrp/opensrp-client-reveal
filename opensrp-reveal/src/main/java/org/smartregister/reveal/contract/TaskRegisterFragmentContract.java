package org.smartregister.reveal.contract;


import android.location.Location;

import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.view.contract.BaseRegisterFragmentContract;

import java.util.List;

/**
 * Created by samuelgithengi on 3/18/19.
 */
public interface TaskRegisterFragmentContract {

    interface Presenter extends BaseRegisterFragmentContract.Presenter {
        void onTasksFound(List<TaskDetails> tasks);

        void onLocationChanged(Location location);

        void onDestroy();
    }

}
