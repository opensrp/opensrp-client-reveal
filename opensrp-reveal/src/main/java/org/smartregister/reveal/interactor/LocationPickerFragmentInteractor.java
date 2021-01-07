package org.smartregister.reveal.interactor;

import org.smartregister.domain.Location;
import org.smartregister.repository.LocationRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.LocationPickerFragmentContract;
import org.smartregister.reveal.util.AppExecutors;

import java.util.List;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class LocationPickerFragmentInteractor implements LocationPickerFragmentContract.Interactor {

    private AppExecutors appExecutors;

    private LocationRepository locationRepository;

    private LocationPickerFragmentContract.Presenter presenter;

    public LocationPickerFragmentInteractor(LocationPickerFragmentContract.Presenter presenter) {
        this.presenter = presenter;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        locationRepository = RevealApplication.getInstance().getLocationRepository();
    }

    @Override
    public void fetchAvailableLocations(List<String> locationIds) {
        Runnable runnable = new Runnable() {
            public void run() {
                List<Location> locations = locationRepository.getLocationsByIds(locationIds, false);

                appExecutors.mainThread().execute(() -> {
                    presenter.onFetchAvailableLocations(locations);
                });
            }
        };

        appExecutors.diskIO().execute(runnable);
    }
}
