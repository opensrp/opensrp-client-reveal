package org.smartregister.reveal.presenter;

import org.smartregister.domain.Location;
import org.smartregister.reveal.contract.EditFociboundaryContract;
import org.smartregister.reveal.interactor.EditFociBoundaryInteractor;
import org.smartregister.reveal.util.EditBoundaryState;

/**
 * Created by Richard Kareko on 6/7/20.
 */

public class EditFociBoundaryPresenter implements EditFociboundaryContract.Presenter {

    private EditFociboundaryContract.Interactor interactor;
    private EditFociboundaryContract.View view;

    public EditFociBoundaryPresenter(EditFociboundaryContract.View view) {
        this.view = view;
        this.interactor = new EditFociBoundaryInteractor(this);
    }

    @Override
    public void onEditFociBoundaryStateChange(EditBoundaryState state) {

    }

    @Override
    public void onDeletePointClicked() {

    }

    @Override
    public void onCancelEditBoundaryChanges() {

    }

    @Override
    public void onSaveEditedBoundary(Location editedFociBoundary) {

    }

    @Override
    public void onEditedBoundarySaved() {
        view.exitEditBoundaryActivity();
    }
}
