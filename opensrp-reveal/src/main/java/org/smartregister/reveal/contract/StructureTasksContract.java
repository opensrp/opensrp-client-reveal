package org.smartregister.reveal.contract;

import android.content.Context;

import org.json.JSONObject;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.RevealJsonFormUtils;

import java.util.List;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public interface StructureTasksContract {

    interface Presenter extends BaseContract.BasePresenter {

        void findTasks(String structureId);

        void onTasksFound(List<StructureTaskDetails> taskDetailsList);

        void onTaskSelected(StructureTaskDetails details);

        void onStructureFound(Location structure, StructureTaskDetails details);

        UserLocationContract.UserLocationPresenter getLocationPresenter();

        void saveJsonForm(String json);
    }

    interface Interactor extends BaseContract.BaseInteractor {

        void findTasks(String structureId, String currentCampaignId);

        void getStructure(StructureTaskDetails details);
    }

    interface View extends UserLocationContract.UserLocationView {

        void setStructure(String structureId);

        void displayToast(String message);

        void showProgressDialog(int title, int message);

        void hideProgressDialog();

        android.location.Location getUserCurrentLocation();

        Context getContext();

        RevealJsonFormUtils getJsonFormUtils();

        void startForm(JSONObject formJSON);

        void setTaskDetailsList(List<StructureTaskDetails> taskDetailsList);

        void updateTask(String taskID, Task.TaskStatus taskStatus, String businessStatus);
    }
}
