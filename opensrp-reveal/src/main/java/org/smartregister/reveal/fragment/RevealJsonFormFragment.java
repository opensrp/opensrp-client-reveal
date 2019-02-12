package org.smartregister.reveal.fragment;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.util.Log;
import android.view.View;

import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.presenters.JsonFormFragmentPresenter;

import org.smartregister.reveal.R;
import org.smartregister.reveal.activity.RevealJsonForm;
import org.smartregister.reveal.interactor.RevealJsonFormInteractor;
import org.smartregister.reveal.presenter.RevealJsonFormFragmentPresenter;
import org.smartregister.reveal.util.Constants;

/**
 * Created by samuelgithengi on 12/13/18.
 */
public class RevealJsonFormFragment extends JsonFormFragment {

    private RevealJsonFormFragmentPresenter presenter;

    @Override
    protected JsonFormFragmentPresenter createPresenter() {
        presenter = new RevealJsonFormFragmentPresenter(this, new RevealJsonFormInteractor());
        return presenter;
    }

    public static RevealJsonFormFragment getFormFragment(String stepName) {
        RevealJsonFormFragment jsonFormFragment = new RevealJsonFormFragment();
        Bundle bundle = new Bundle();
        bundle.putString(JsonFormConstants.JSON_FORM_KEY.STEPNAME, stepName);
        jsonFormFragment.setArguments(bundle);
        return jsonFormFragment;
    }

    @Override
    public void onViewCreated(View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        setupMargins(view);
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == io.ona.kujaku.utils.Constants.RequestCode.LOCATION_SETTINGS) {
            Log.d(RevealJsonFormFragment.class.getName(), "on onActivityResult LOCATION_SETTINGS");
          /*  if (resultCode == RESULT_OK) {
                listTaskPresenter.waitForUserLocation();
            } else if (resultCode == RESULT_CANCELED) {
                listTaskPresenter.onGetUserLocationFailed();
            }
            hasRequestedLocation = false;*/
        }
    }

    private void setupMargins(View view) {
        if (getArguments() != null) {
            String stepName = getArguments().getString(JsonFormConstants.STEPNAME);
            if (getStep(stepName).optBoolean(Constants.JsonForm.NO_PADDING)) {
                view.findViewById(R.id.main_layout).setPadding(0, 0, 0, 0);
            }
        }
    }

    public RevealJsonFormFragmentPresenter getPresenter() {
        return presenter;
    }
}
