package org.smartregister.reveal.presenter;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.annotation.VisibleForTesting;

import com.mapbox.geojson.Feature;

import org.json.JSONArray;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Task;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.interactor.StructureTasksInteractor;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.PreferencesUtil;

import java.lang.ref.WeakReference;
import java.util.List;

import static org.smartregister.reveal.contract.StructureTasksContract.Interactor;
import static org.smartregister.reveal.contract.StructureTasksContract.Presenter;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class StructureTasksPresenter extends BaseFormFragmentPresenter implements Presenter {

    private WeakReference<StructureTasksContract.View> view;

    private Interactor interactor;

    private PreferencesUtil prefsUtil;


    public StructureTasksPresenter(StructureTasksContract.View view, Context context) {
        this(view, context, null, PreferencesUtil.getInstance());
        interactor = new StructureTasksInteractor(this);
    }

    @VisibleForTesting
    public StructureTasksPresenter(StructureTasksContract.View view, Context context, Interactor interactor, PreferencesUtil prefsUtil) {
        super(view, context);
        this.view = new WeakReference<>(view);
        this.interactor = interactor;
        this.prefsUtil = prefsUtil;
    }

    @Override
    public void findTasks(String structureId) {
        interactor.findTasks(structureId, prefsUtil.getCurrentPlanId());
    }

    @Override
    public void onTasksFound(List<StructureTaskDetails> taskDetailsList) {
        getView().setTaskDetailsList(taskDetailsList);
    }

    @Override
    public void onTaskSelected(StructureTaskDetails details) {
        if (details != null) {
            if (Task.TaskStatus.COMPLETED.name().equals(details.getTaskStatus())) {
                getView().displayToast("Task Completed");
            } else {
                getView().showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
                interactor.getStructure(details);
            }
        }
    }

    public StructureTasksContract.View getView() {
        return view.get();
    }

    @Override
    public void saveJsonForm(String json) {
        getView().showProgressDialog(R.string.saving_title, R.string.saving_message);
        interactor.saveJsonForm(json);
    }

    @Override
    public void onFormSaved(@NonNull String structureId, String taskID, @NonNull Task.TaskStatus taskStatus, @NonNull String businessStatus, String interventionType) {
        getView().hideProgressDialog();
        getView().updateTask(taskID, taskStatus, businessStatus);
    }

    @Override
    public void onStructureAdded(Feature feature, JSONArray featureCoordinates) {//not used
    }

    @Override
    public void onFormSaveFailure(String eventType) {
        getView().hideProgressDialog();//register will refresh on resume
    }

    @Override
    public void onFamilyFound(CommonPersonObjectClient finalFamily) {//not used
    }
}
