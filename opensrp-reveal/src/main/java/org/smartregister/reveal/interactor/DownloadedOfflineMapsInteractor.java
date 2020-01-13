package org.smartregister.reveal.interactor;

import android.support.v4.util.Pair;

import com.mapbox.mapboxsdk.offline.OfflineRegion;

import org.smartregister.domain.Location;
import org.smartregister.repository.LocationRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.DownloadedOfflineMapsContract;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.util.AppExecutors;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class DownloadedOfflineMapsInteractor implements DownloadedOfflineMapsContract.Interactor {

    private AppExecutors appExecutors;

    private LocationRepository locationRepository;

    private DownloadedOfflineMapsContract.Presenter presenter;

    public DownloadedOfflineMapsInteractor(DownloadedOfflineMapsContract.Presenter presenter) {
        this.presenter = presenter;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        locationRepository = RevealApplication.getInstance().getLocationRepository();
    }

    @Override
    public void fetchLocationsWithOfflineMapDownloads(Pair<List<String>, Map<String, OfflineRegion>> offlineRegionInfo) {

        List<Location> operationalAreas = locationRepository.getLocationsByIds(offlineRegionInfo.first);

        appExecutors.mainThread().execute(new Runnable() {
            @Override
            public void run() {
                presenter.onOAsWithOfflineDownloadsFetched(populateOfflineMapModelList(operationalAreas, offlineRegionInfo.second));
            }
        });

    }

    private List<OfflineMapModel>  populateOfflineMapModelList(List<Location> locations, Map<String, OfflineRegion> offlineRegionMap) {
        List<OfflineMapModel> offlineMapModels = new ArrayList<>();
        for (Location location: locations) {
            OfflineMapModel offlineMapModel = new OfflineMapModel();
            offlineMapModel.setLocation(location);
            offlineMapModel.setOfflineRegion(offlineRegionMap.get(location.getId()));
            offlineMapModels.add(offlineMapModel);
        }

        return offlineMapModels;
    };
}
