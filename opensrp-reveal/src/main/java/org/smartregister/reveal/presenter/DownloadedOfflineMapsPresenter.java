package org.smartregister.reveal.presenter;

import org.smartregister.reveal.contract.DownloadedOfflineMapsContract;
import org.smartregister.reveal.interactor.DownloadedOfflineMapsInteractor;
import org.smartregister.reveal.model.OfflineMapModel;

import java.util.List;

public class DownloadedOfflineMapsPresenter implements DownloadedOfflineMapsContract.Presenter {

    private DownloadedOfflineMapsContract.View view;
    private DownloadedOfflineMapsContract.Interactor interactor;

    public DownloadedOfflineMapsPresenter(DownloadedOfflineMapsContract.View view) {
        this.view = view;
        this.interactor = new DownloadedOfflineMapsInteractor(this);
    }

    @Override
    public void onDeleteDownloadMap(List<OfflineMapModel> offlineMapModels) {
        view.deleteDownloadedOfflineMaps();
    }

    @Override
    public void fetchOAsWithOfflineDownloads(List<String> locationIds) {
        interactor.fetchLocationsWithOfflineMapDownloads(locationIds);
    }

    @Override
    public void onOAsWithOfflineDownloadsFetched(List<OfflineMapModel> downloadedOfflineMapModelList) {
        view.setDownloadedOfflineMapModelList(downloadedOfflineMapModelList);
    }
}
