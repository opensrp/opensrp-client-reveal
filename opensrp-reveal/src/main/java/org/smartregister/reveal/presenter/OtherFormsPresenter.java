package org.smartregister.reveal.presenter;

import androidx.annotation.NonNull;

import com.mapbox.geojson.Feature;

import org.json.JSONArray;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Task;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.OtherFormsContract;
import org.smartregister.reveal.interactor.OtherFormsInteractor;

public class OtherFormsPresenter implements OtherFormsContract.Presenter {

    private OtherFormsContract.Interactor interactor;

    private OtherFormsContract.View view;

    public OtherFormsPresenter(OtherFormsContract.View view) {
        this.view = view;
        interactor = new OtherFormsInteractor(this);
    }

    @Override
    public void saveJsonForm(String json) {
        view.showProgressDialog(R.string.saving_dialog_title);
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
