package org.smartregister.reveal.contract;

import org.smartregister.reveal.model.OfflineMapModel;

public interface OfflineMapDownloadCallback {

    void onMapDownloaded(OfflineMapModel offlineMapModel);

    void onOfflineMapDeleted(OfflineMapModel offlineMapModel);
}
