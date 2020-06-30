package org.smartregister.reveal.contract;

import androidx.core.util.Pair;

import com.mapbox.mapboxsdk.offline.OfflineRegion;

import org.smartregister.reveal.model.OfflineMapModel;

import java.util.List;
import java.util.Map;

public interface DownloadedOfflineMapsContract extends OfflineMapsFragmentContract {

    interface Presenter {

        void onDeleteDownloadMap(List<OfflineMapModel> offlineMapModels);

        void fetchOAsWithOfflineDownloads(Pair<List<String>, Map<String, OfflineRegion>> offlineRegionInfo);

        void onOAsWithOfflineDownloadsFetched(List<OfflineMapModel> downloadedOfflineMapModelList);
    }

    interface View  {

        void setDownloadedOfflineMapModelList(List<OfflineMapModel> downloadedOfflineMapModelList);

        void deleteDownloadedOfflineMaps();

    }

    interface Interactor {

        void fetchLocationsWithOfflineMapDownloads(Pair<List<String>, Map<String, OfflineRegion>> offlineRegionInfo);
    }
}
