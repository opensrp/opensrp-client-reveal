package org.smartregister.reveal.contract;

import org.smartregister.domain.Location;
import org.smartregister.reveal.util.EditBoundaryState;

/**
 * Created by Richard Kareko on 6/7/20.
 */

public interface EditFociboundaryContract {
    interface View {

        void toggleButtons(EditBoundaryState state);

        void exitEditBoundaryActivity();
    }

    interface Presenter {

        void onEditFociBoundaryStateChange(EditBoundaryState state);

        void onDeletePointClicked();

        void onCancelEditBoundaryChanges();

        void onSaveEditedBoundary(Location editedFociBoundary);

        void onEditedBoundarySaved();
    }

    interface Interactor {

        void saveLocation(Location editedFociBoundary);

    }
}
