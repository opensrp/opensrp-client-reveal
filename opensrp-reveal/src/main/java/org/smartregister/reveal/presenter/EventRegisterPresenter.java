package org.smartregister.reveal.presenter;

import androidx.annotation.NonNull;

import com.mapbox.geojson.Feature;

import org.json.JSONArray;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Task;
import org.smartregister.reveal.contract.EventRegisterActivityContract;
import org.smartregister.reveal.interactor.EventRegisterInteractor;

/**
 * Created by Richard Kareko on 7/31/20.
 */

public class EventRegisterPresenter extends BaseRegisterPresenter implements EventRegisterActivityContract.Presenter {

    private EventRegisterActivityContract.Interactor interactor;

    private EventRegisterActivityContract.View view;

    public EventRegisterPresenter(EventRegisterActivityContract.View view) {
        super(view);
        this.view = view;
        interactor = new EventRegisterInteractor(this);
    }

    @Override
    public void saveJsonForm(String json) {
        interactor.saveJsonForm(json);
    }

    @Override
    public void onFormSaved(@NonNull String structureId, String taskID, @NonNull Task.TaskStatus taskStatus, @NonNull String businessStatus, String interventionType) {
        view.hideProgressDialog();
    }

    @Override
    public void onStructureAdded(Feature feature, JSONArray featureCoordinates, double zoomlevel) {
        // Do nothing
    }

    @Override
    public void onFormSaveFailure(String eventType) {
        view.hideProgressDialog();
    }

    @Override
    public void onFamilyFound(CommonPersonObjectClient finalFamily) {
        // Do nothing
    }
}
