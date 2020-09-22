package org.smartregister.reveal.contract;

import org.smartregister.domain.Location;

import java.util.List;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public interface LocationPickerFragmentContract {

    interface Presenter {

        void fetchAvailableLocations(List<String> locationIds);

        void onFetchAvailableLocations(List<Location> locations);

    }

    interface View {

        void setAvailableLocations(List<Location> locations);
    }

    interface Interactor {
        void fetchAvailableLocations(List<String> locationIds);
    }
}
