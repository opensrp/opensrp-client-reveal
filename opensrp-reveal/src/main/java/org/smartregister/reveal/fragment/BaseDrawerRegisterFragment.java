package org.smartregister.reveal.fragment;

import android.view.View;

import org.smartregister.dto.UserAssignmentDTO;
import org.smartregister.receiver.ValidateAssignmentReceiver;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.view.fragment.BaseRegisterFragment;

/**
 * Created by samuelgithengi on 9/21/20.
 */
public abstract class BaseDrawerRegisterFragment extends BaseRegisterFragment implements ValidateAssignmentReceiver.UserAssignmentListener {

    protected BaseDrawerContract.View drawerView;

    @Override
    public void setupViews(View view) {
        super.setupViews(view);
        ValidateAssignmentReceiver.getInstance().addListener(this);
    }

    @Override
    public void onUserAssignmentRevoked(UserAssignmentDTO userAssignmentDTO) {
        drawerView.onResume();
    }

    @Override
    public void onPause() {
        super.onPause();
        ValidateAssignmentReceiver.getInstance().removeLister(this);
    }
}
