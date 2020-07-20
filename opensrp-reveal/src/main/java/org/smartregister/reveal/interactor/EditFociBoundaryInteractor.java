package org.smartregister.reveal.interactor;

import org.smartregister.domain.Location;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.LocationRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.EditFociboundaryContract;
import org.smartregister.reveal.util.AppExecutors;

/**
 * Created by Richard Kareko on 6/7/20.
 */

public class EditFociBoundaryInteractor implements EditFociboundaryContract.Interactor {

    private AppExecutors appExecutors;

    private LocationRepository locationRepository;

    private EditFociboundaryContract.Presenter presenter;

    public EditFociBoundaryInteractor(EditFociboundaryContract.Presenter presenter) {
        this.presenter = presenter;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        locationRepository = RevealApplication.getInstance().getLocationRepository();
    }

    @Override
    public void saveLocation(Location editedFociBoundary) {
        appExecutors.diskIO().execute(() -> {
            editedFociBoundary.getProperties().setUsername(RevealApplication.getInstance().getContext().allSharedPreferences().fetchRegisteredANM());
            editedFociBoundary.setSyncStatus(BaseRepository.TYPE_Unsynced);
            locationRepository.addOrUpdate(editedFociBoundary);

            appExecutors.mainThread().execute(() -> {
                presenter.onEditedBoundarySaved();
            });
        });

    }
}
