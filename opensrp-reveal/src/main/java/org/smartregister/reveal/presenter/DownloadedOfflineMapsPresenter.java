package org.smartregister.reveal.presenter;

import org.smartregister.reveal.contract.DownloadedOfflineMapsContract;
import org.smartregister.reveal.model.OfflineMapModel;

import java.util.List;

public class DownloadedOfflineMapsPresenter implements DownloadedOfflineMapsContract.Presenter {

    private DownloadedOfflineMapsContract.View view;

    public DownloadedOfflineMapsPresenter(DownloadedOfflineMapsContract.View view) {
        this.view = view;
    }

    @Override
    public void onDeleteDownloadMap(List<OfflineMapModel> offlineMapModels) {
        if (offlineMapModels == null || offlineMapModels.isEmpty()) {
            return;
        }

        view.deleteDownloadedOfflineMaps();
    }
}
