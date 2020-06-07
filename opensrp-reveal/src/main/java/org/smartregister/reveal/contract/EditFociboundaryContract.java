package org.smartregister.reveal.contract;

import android.content.Context;

import org.smartregister.domain.Location;
import org.smartregister.reveal.util.EditBoundaryState;

/**
 * Created by Richard Kareko on 6/7/20.
 */

public interface EditFociboundaryContract {
    interface View {

        void toggleButtons(EditBoundaryState state);

        void exitEditBoundaryActivity();

        Context getContext();

        void deletePoint(android.view.View view);

    }

    interface Presenter {

        void onEditFociBoundaryStateChange(EditBoundaryState state);

        void onDeletePoint(android.view.View view);

        void onCancelEditBoundaryChanges();

        void onSaveEditedBoundary(Location editedFociBoundary);

        void onEditedBoundarySaved();

        void onSavePoint();

        void displayDeletePointDialog(android.view.View view);

        void displayDiscardChangesDialog();
    }

    interface Interactor {

        void saveLocation(Location editedFociBoundary);

    }
}
