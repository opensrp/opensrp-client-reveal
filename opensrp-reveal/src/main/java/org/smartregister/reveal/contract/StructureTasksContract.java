package org.smartregister.reveal.contract;

import android.content.Context;

import org.smartregister.domain.Task;
<<<<<<< HEAD
import org.smartregister.domain.db.Event;
=======
import org.smartregister.reveal.model.EventTask;
>>>>>>> master
import org.smartregister.reveal.model.StructureTaskDetails;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public interface StructureTasksContract {

    interface Presenter extends BaseContract.BasePresenter, BaseFormFragmentContract.Presenter {

        void findTasks(String structureId);

        void onTasksFound(List<StructureTaskDetails> taskDetailsList, StructureTaskDetails incompleteIndexCase);

        void onTaskSelected(StructureTaskDetails details, boolean isEdit);

        void saveJsonForm(String json);

        void onDetectCase();

        void onIndexConfirmationFormSaved(String taskID, Task.TaskStatus taskStatus, String businessStatus, Set<Task> removedTasks);

        void onEventFound(Event event, boolean isEdited);
    }

    interface Interactor extends BaseContract.BaseInteractor {

        void findTasks(String structureId, String currentPlanId, String operationalAreaId);

        void getStructure(StructureTaskDetails details);

        void findLastEvent(StructureTaskDetails taskDetails);

        Map<String, EventTask> findEventsPerTask(List<Task> tasks);
    }

    interface View extends UserLocationContract.UserLocationView, BaseFormFragmentContract.View {

        void setStructure(String structureId);

        void showProgressDialog(int title, int message);

        void hideProgressDialog();

        android.location.Location getUserCurrentLocation();

        Context getContext();

        void setTaskDetailsList(List<StructureTaskDetails> taskDetailsList);

        void updateTask(String taskID, Task.TaskStatus taskStatus, String businessStatus);

        void displayDetectCaseButton();

        void hideDetectCaseButton();

        void updateNumberOfTasks();

        void updateTasks(String taskID, Task.TaskStatus taskStatus, String businessStatus, Set<Task> removedTasks);
    }
}
