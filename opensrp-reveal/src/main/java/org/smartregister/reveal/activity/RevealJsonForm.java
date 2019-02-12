package org.smartregister.reveal.activity;

import android.content.Intent;

import com.vijay.jsonwizard.activities.JsonFormActivity;
import com.vijay.jsonwizard.constants.JsonFormConstants;

import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.RevealJsonFormFragment;

import io.ona.kujaku.utils.Constants;

/**
 * Created by samuelgithengi on 12/13/18.
 */
public class RevealJsonForm extends JsonFormActivity {

    private RevealJsonFormFragment formFragment;

    private boolean requestedLocation;

    @Override
    public void initializeFormFragment() {
        RevealJsonFormFragment revealJsonFormFragment = RevealJsonFormFragment.getFormFragment(JsonFormConstants.FIRST_STEP_NAME);
        getSupportFragmentManager().beginTransaction()
                .add(R.id.container, revealJsonFormFragment).commit();
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == Constants.RequestCode.LOCATION_SETTINGS && requestedLocation) {
            if (resultCode == RESULT_OK) {
                formFragment.getPresenter().waitForUserLocation();
            } else if (resultCode == RESULT_CANCELED) {
                formFragment.getPresenter().onGetUserLocationFailed();
            }
            requestedLocation = false;
        }
    }

    @Override
    public void onAttachFragment(android.support.v4.app.Fragment fragment) {
        super.onAttachFragment(fragment);
        if (fragment instanceof RevealJsonFormFragment) {
            formFragment = (RevealJsonFormFragment) fragment;
        }
    }

    public void setRequestedLocation(boolean requestedLocation) {
        this.requestedLocation = requestedLocation;
    }

    public boolean isRequestedLocation() {
        return requestedLocation;
    }
}
