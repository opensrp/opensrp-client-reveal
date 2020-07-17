package org.smartregister.reveal.presenter;

import android.content.DialogInterface;
import android.view.View;

import org.smartregister.domain.Location;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.EditFociboundaryContract;
import org.smartregister.reveal.interactor.EditFociBoundaryInteractor;
import org.smartregister.reveal.util.AlertDialogUtils;
import org.smartregister.reveal.util.EditBoundaryState;

import static android.content.DialogInterface.BUTTON_POSITIVE;

/**
 * Created by Richard Kareko on 6/7/20.
 */

public class EditFociBoundaryPresenter implements EditFociboundaryContract.Presenter {

    private EditFociboundaryContract.Interactor interactor;
    private EditFociboundaryContract.View editFociBoundaryview;

    public EditFociBoundaryPresenter(EditFociboundaryContract.View view) {
        this.editFociBoundaryview = view;
        this.interactor = new EditFociBoundaryInteractor(this);
    }

    @Override
    public void onDeletePoint(View view) {
        displayDeletePointDialog(view);
    }

    @Override
    public void onCancelEditBoundaryChanges() {
        displayDiscardChangesDialog();
    }

    @Override
    public void onSaveEditedBoundary(Location editedFociBoundary) {
        interactor.saveLocation(editedFociBoundary);
    }

    @Override
    public void onEditedBoundarySaved() {
        editFociBoundaryview.exitEditBoundaryActivity();
    }

    @Override
    public void onSavePoint() {
        editFociBoundaryview.toggleButtons(EditBoundaryState.FINISHED);
        editFociBoundaryview.setToolbarTitle(R.string.edit_boundary);
    }

    @Override
    public void onEditPoint() {
        editFociBoundaryview.toggleButtons(EditBoundaryState.EDITTING);
        editFociBoundaryview.displaySnackBar(R.string.drag_selected_point_msg);
        editFociBoundaryview.setToolbarTitle(R.string.change_point);
    }

    @Override
    public void displayDeletePointDialog(View view) {
        AlertDialogUtils.displayNotificationWithCallback(editFociBoundaryview.getContext(), R.string.delete_point_title,
                R.string.delete_point_msg, R.string.delete, R.string.cancel, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        if (which == BUTTON_POSITIVE) {
                            editFociBoundaryview.deletePoint(view);
                        }
                        dialog.dismiss();
                    }
                });
    }

    @Override
    public void displayDiscardChangesDialog() {
        AlertDialogUtils.displayNotificationWithCallback(editFociBoundaryview.getContext(), R.string.discard_changes_title,
                R.string.discard_changes_msg, R.string.discard, R.string.cancel, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        if (which == BUTTON_POSITIVE) {
                            editFociBoundaryview.exitEditBoundaryActivity();
                        }
                        dialog.dismiss();
                    }
                });
    }
}
