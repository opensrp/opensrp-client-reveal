package org.smartregister.reveal.interactor;

import org.smartregister.domain.Location;
import org.smartregister.repository.LocationRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.AvailableOfflineMapsContract;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.util.AppExecutors;

import java.util.ArrayList;
import java.util.List;

public class AvailableOfflineMapsInteractor implements AvailableOfflineMapsContract.Interactor {

    private AppExecutors appExecutors;

    private LocationRepository locationRepository;

    private AvailableOfflineMapsContract.Presenter presenter;

    public AvailableOfflineMapsInteractor(AvailableOfflineMapsContract.Presenter presenter) {
        this.presenter = presenter;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        locationRepository = RevealApplication.getInstance().getLocationRepository();
    }


    @Override
    public void fetchAvailableOAsForMapDownLoad(List<String> locationIds) {

        List<Location> operationalAreas = locationRepository.getLocationsByIds(locationIds, false);

        appExecutors.mainThread().execute(() -> {
            presenter.onFetchAvailableOAsForMapDownLoad(populateOfflineMapModelList(operationalAreas));
        });

    }

    public List<OfflineMapModel>  populateOfflineMapModelList(List<Location> locations) {
        List<OfflineMapModel> offlineMapModels = new ArrayList<>();
        for (Location location: locations) {
            OfflineMapModel offlineMapModel = new OfflineMapModel();
            offlineMapModel.setLocation(location);
            offlineMapModels.add(offlineMapModel);
        }

        return offlineMapModels;
    }

}
