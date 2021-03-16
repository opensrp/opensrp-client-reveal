package org.smartregister.reveal.presenter;

import android.content.DialogInterface;
import android.view.View;

import org.smartregister.domain.Location;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.EditFociboundaryContract;
import org.smartregister.reveal.interactor.EditFociBoundaryInteractor;
import org.smartregister.reveal.util.AlertDialogUtils;
import org.smartregister.reveal.util.EditBoundaryState;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;

import static android.content.DialogInterface.BUTTON_POSITIVE;

/**
 * Created by Richard Kareko on 6/7/20.
 */

public class EditFociBoundaryPresenter implements EditFociboundaryContract.Presenter {

    private EditFociboundaryContract.Interactor interactor;
    private EditFociboundaryContract.View editFociBoundaryView;

    public EditFociBoundaryPresenter(EditFociboundaryContract.View view) {
        this.editFociBoundaryView = view;
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
        editFociBoundaryView.exitEditBoundaryActivity();
        Utils.evictCache(PreferencesUtil.getInstance().getCurrentOperationalArea());
    }

    @Override
    public void onSavePoint() {
        editFociBoundaryView.toggleButtons(EditBoundaryState.FINISHED);
        editFociBoundaryView.setToolbarTitle(R.string.edit_boundary);
    }

    @Override
    public void onEditPoint() {
        editFociBoundaryView.toggleButtons(EditBoundaryState.EDITTING);
        editFociBoundaryView.displaySnackBar(R.string.drag_selected_point_msg);
        editFociBoundaryView.setToolbarTitle(R.string.change_point);
    }

    @Override
    public void displayDeletePointDialog(View view) {
        AlertDialogUtils.displayNotificationWithCallback(editFociBoundaryView.getContext(), R.string.delete_point_title,
                R.string.delete_point_msg, R.string.delete, R.string.cancel, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        if (which == BUTTON_POSITIVE) {
                            editFociBoundaryView.deletePoint(view);
                        }
                        dialog.dismiss();
                    }
                });
    }

    @Override
    public void displayDiscardChangesDialog() {
        AlertDialogUtils.displayNotificationWithCallback(editFociBoundaryView.getContext(), R.string.discard_changes_title,
                R.string.discard_changes_msg, R.string.discard, R.string.cancel_no, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        if (which == BUTTON_POSITIVE) {
                            editFociBoundaryView.exitEditBoundaryActivity();
                        }
                        dialog.dismiss();
                    }
                });
    }
}
