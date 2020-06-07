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
    public void onEditFociBoundaryStateChange(EditBoundaryState state) {

    }

    @Override
    public void onDeletePoint(View view) {
        displayDeletePointDialog(view);
    }

    @Override
    public void onCancelEditBoundaryChanges() {

    }

    @Override
    public void onSaveEditedBoundary(Location editedFociBoundary) {

    }

    @Override
    public void onEditedBoundarySaved() {
        editFociBoundaryview.exitEditBoundaryActivity();
    }

    @Override
    public void onSavePoint() {
        editFociBoundaryview.toggleButtons(EditBoundaryState.FINISHED);
    }

    @Override
    public void displayDeletePointDialog(View view) {
        AlertDialogUtils.displayNotificationWithCallback(view.getContext(), R.string.delete_point_title,
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

    }
}
