package org.smartregister.reveal.interactor;

import org.smartregister.domain.Location;
import org.smartregister.repository.LocationRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.AvailableOfflineMapsContract;
import org.smartregister.reveal.util.AppExecutors;

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
    public void fetchOperationalAreas() {

        List<Location> operationalAreas = locationRepository.getAllLocations();

        appExecutors.mainThread().execute(new Runnable() {
            @Override
            public void run() {
                presenter.onFetchOperationalAreas(operationalAreas);
            }
        });

    }
}
