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

import java.util.List;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterPresenter implements TaskRegisterContract.Presenter, BaseRegisterContract.Presenter {

    private TaskRegisterInteractor taskRegisterInteractor;

    private BaseRegisterContract.View view;

    public TaskRegisterPresenter(BaseRegisterContract.View view) {
        this.view = view;
        taskRegisterInteractor = new TaskRegisterInteractor(this);
    }

    @Override
    public void registerViewConfigurations(List<String> viewIdentifiers) {
        taskRegisterInteractor.registerViewConfigurations(viewIdentifiers);
    }

    @Override
    public void unregisterViewConfiguration(List<String> viewIdentifiers) {
        taskRegisterInteractor.unregisterViewConfiguration(viewIdentifiers);
    }

    @Override
    public void onDestroy(boolean isChangingConfiguration) {
        taskRegisterInteractor.cleanupResources();
    }

    @Override
    public void updateInitials() {//do nothing
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
