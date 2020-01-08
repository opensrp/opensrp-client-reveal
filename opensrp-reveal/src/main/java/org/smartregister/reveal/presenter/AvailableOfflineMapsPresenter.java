package org.smartregister.reveal.presenter;

import org.smartregister.domain.Location;
import org.smartregister.reveal.contract.AvailableOfflineMapsContract;
import org.smartregister.reveal.interactor.AvailableOfflineMapsInteractor;
import org.smartregister.reveal.model.OfflineMapModel;

import java.util.ArrayList;
import java.util.List;

public class AvailableOfflineMapsPresenter implements AvailableOfflineMapsContract.Presenter {

    private AvailableOfflineMapsContract.Interactor interactor;
    private AvailableOfflineMapsContract.View view;

    public AvailableOfflineMapsPresenter(AvailableOfflineMapsContract.View view) {
        this.view = view;
        this.interactor = new AvailableOfflineMapsInteractor(this);
    }

    @Override
    public void onDownloadAreaSelected(OfflineMapModel offlineMapModel) {
        view.updateOperationalAreasToDownload(offlineMapModel.getLocation());
    }

    @Override
    public void fetchOperationalAreas() {
        interactor.fetchOperationalAreas();
    }

    @Override
    public void onFetchOperationalAreas(List<Location> locations) {

        List<OfflineMapModel> offlineMapModels = populateOfflineMapModelList(locations);
        view.setOfflineMapModelList(offlineMapModels);
    }

    @Override
    public void onDownloadStarted(String operationalAreaname) {
        view.disableCheckBox(operationalAreaname);

    }

    @Override
    public void onDownloadComplete(String operationalAreaName) {
        view.moveDownloadedOAToDownloadedList(operationalAreaName);
    }

    private List<OfflineMapModel>  populateOfflineMapModelList(List<Location> locations) {
        List<OfflineMapModel> offlineMapModels = new ArrayList<>();
        for (Location location: locations) {
            OfflineMapModel offlineMapModel = new OfflineMapModel();
            offlineMapModel.setLocation(location);
            offlineMapModels.add(offlineMapModel);
        }

        return offlineMapModels;
    };

}
