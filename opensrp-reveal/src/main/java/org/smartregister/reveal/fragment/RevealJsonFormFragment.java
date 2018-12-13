package org.smartregister.reveal.fragment;

import android.os.Bundle;

import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.presenters.JsonFormFragmentPresenter;

import org.smartregister.reveal.interactor.RevealJsonFormInteractor;

/**
 * Created by samuelgithengi on 12/13/18.
 */
public class RevealJsonFormFragment extends JsonFormFragment {

    @Override
    protected JsonFormFragmentPresenter createPresenter() {
        return new JsonFormFragmentPresenter(this, new RevealJsonFormInteractor());
    }

    public static RevealJsonFormFragment getFormFragment(String stepName) {
        RevealJsonFormFragment jsonFormFragment = new RevealJsonFormFragment();
        Bundle bundle = new Bundle();
        bundle.putString(JsonFormConstants.JSON_FORM_KEY.STEPNAME, stepName);
        jsonFormFragment.setArguments(bundle);
        return jsonFormFragment;
    }

}
