package org.smartregister.reveal.presenter;

import androidx.annotation.NonNull;

import com.mapbox.geojson.Feature;

import org.json.JSONArray;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Task;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.TaskRegisterContract;
import org.smartregister.reveal.interactor.TaskRegisterInteractor;
import org.smartregister.view.contract.BaseRegisterContract;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterPresenter extends BaseRegisterPresenter implements TaskRegisterContract.Presenter, BaseRegisterContract.Presenter {

    private BaseRegisterContract.View view;

    public TaskRegisterPresenter(BaseRegisterContract.View view) {
        super(view);
        this.view = view;
        taskRegisterInteractor = new TaskRegisterInteractor(this);
    }

    @Override
    public void saveJsonForm(String json) {
        view.showProgressDialog(R.string.saving_dialog_title);
        taskRegisterInteractor.saveJsonForm(json);
    }

    @Override
    public void onFamilyFound(CommonPersonObjectClient finalFamily) {//not used
    }

    @Override
    public void onFormSaved(@NonNull String structureId, String taskID, @NonNull Task.TaskStatus taskStatus, @NonNull String businessStatus, String interventionType) {
        view.hideProgressDialog();//register will refresh on resume
    }

    @Override
    public void onStructureAdded(Feature feature, JSONArray featureCoordinates, double zoomlevel) {
        view.hideProgressDialog();//register will refresh on resume
    }

    @Override
    public void onFormSaveFailure(String eventType) {
        view.hideProgressDialog();//register will refresh on resume
    }
}
