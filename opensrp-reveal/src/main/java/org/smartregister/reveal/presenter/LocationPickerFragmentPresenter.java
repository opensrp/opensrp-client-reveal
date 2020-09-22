package org.smartregister.reveal.presenter;

import org.smartregister.domain.Location;
import org.smartregister.reveal.contract.LocationPickerFragmentContract;
import org.smartregister.reveal.interactor.LocationPickerFragmentInteractor;

import java.util.List;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class LocationPickerFragmentPresenter implements LocationPickerFragmentContract.Presenter {

    private LocationPickerFragmentContract.Interactor interactor;
    private LocationPickerFragmentContract.View view;

    public LocationPickerFragmentPresenter(LocationPickerFragmentContract.View view) {
        this.view = view;
        this.interactor = new LocationPickerFragmentInteractor(this);
    }

    @Override
    public void fetchAvailableLocations(List<String> locationIds) {
        interactor.fetchAvailableLocations(locationIds);
    }

    @Override
    public void onFetchAvailableLocations(List<Location> locations) {
        view.setAvailableLocations(locations);
    }

}
