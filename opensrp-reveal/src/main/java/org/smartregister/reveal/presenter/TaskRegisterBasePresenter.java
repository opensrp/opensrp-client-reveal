package org.smartregister.reveal.presenter;

import android.support.annotation.NonNull;

import com.mapbox.geojson.Feature;

import org.json.JSONArray;
import org.smartregister.domain.Task;
import org.smartregister.reveal.contract.TaskRegisterContract;
import org.smartregister.reveal.interactor.TaskRegisterInteractor;
import org.smartregister.view.contract.BaseRegisterContract;

import java.util.List;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterBasePresenter implements TaskRegisterContract.BasePresenter, BaseRegisterContract.Presenter {


    private TaskRegisterInteractor taskRegisterInteractor;

    public TaskRegisterBasePresenter() {
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
        taskRegisterInteractor.saveJsonForm(json);
    }

    @Override
    public void onSprayFormSaved(@NonNull String structureId, @NonNull String taskIdentifier, @NonNull Task.TaskStatus taskStatus, @NonNull String businessStatus) {
    }

    @Override
    public void onStructureAdded(Feature feature, JSONArray featureCoordinates) {
    }

    @Override
    public void onFormSaveFailure(String eventType) {
    }

    @Override
    public void onMosquitoCollectionFormSaved() {//refresh register
    }
}
