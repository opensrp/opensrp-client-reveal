package org.smartregister.reveal.presenter;

import org.smartregister.reveal.contract.AvailableOfflineMapsContract;
import org.smartregister.reveal.interactor.AvailableOfflineMapsInteractor;
import org.smartregister.reveal.model.OfflineMapModel;

import java.util.List;

public class AvailableOfflineMapsPresenter implements AvailableOfflineMapsContract.Presenter {

    private AvailableOfflineMapsContract.Interactor interactor;
    private AvailableOfflineMapsContract.View view;

    public AvailableOfflineMapsPresenter(AvailableOfflineMapsContract.View view) {
        this.view = view;
        this.interactor = new AvailableOfflineMapsInteractor(this);
    }

    @Override
    public void fetchAvailableOAsForMapDownLoad(List<String> locationIds) {
        interactor.fetchAvailableOAsForMapDownLoad(locationIds);
    }

    @Override
    public void onFetchAvailableOAsForMapDownLoad(List<OfflineMapModel> offlineMapModels) {
        view.setOfflineMapModelList(offlineMapModels, true);
    }

    @Override
    public void onDownloadStarted(String operationalAreaId) {
        view.disableCheckBox(operationalAreaId);

    }

    @Override
    public void onDownloadComplete(String operationalAreaId) {
        view.moveDownloadedOAToDownloadedList(operationalAreaId);
    }

}
