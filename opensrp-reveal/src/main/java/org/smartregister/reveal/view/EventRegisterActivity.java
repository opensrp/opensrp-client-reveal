package org.smartregister.reveal.view;

import android.content.Intent;
import android.view.View;

import androidx.annotation.VisibleForTesting;

import org.json.JSONObject;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.EventRegisterActivityContract;
import org.smartregister.reveal.fragment.EventRegisterFragment;
import org.smartregister.reveal.presenter.EventRegisterPresenter;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.view.fragment.BaseRegisterFragment;

import java.util.Collections;
import java.util.List;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.JSON_FORM_PARAM_JSON;
import static org.smartregister.reveal.util.Constants.RequestCode.REQUEST_CODE_GET_JSON;

/**
 * Created by samuelgithengi on 7/30/20.
 */
public class EventRegisterActivity extends BaseRevealRegisterActivity implements EventRegisterActivityContract.View  {

    private RevealJsonFormUtils jsonFormUtils;

    @Override
    protected void initializePresenter() {
        presenter = new EventRegisterPresenter(this);
    }

    @Override
    protected BaseRegisterFragment getRegisterFragment() {
        jsonFormUtils = new RevealJsonFormUtils();
        EventRegisterFragment fragment =  new EventRegisterFragment();
        fragment.setJsonFormUtils(jsonFormUtils);
        return fragment;
    }

    @Override
    protected void onActivityResultExtended(int requestCode, int resultCode, Intent data) {
        if (requestCode == REQUEST_CODE_GET_JSON && resultCode == RESULT_OK && data.hasExtra(JSON_FORM_PARAM_JSON)) {
            String json = data.getStringExtra(JSON_FORM_PARAM_JSON);
            Timber.d(json);
            getPresenter().saveJsonForm(json);
        } else {
            mBaseFragment.onActivityResult(requestCode, resultCode, data);
        }
    }

    @Override
    public List<String> getViewIdentifiers() {
        return Collections.singletonList(Constants.EventsRegister.VIEW_IDENTIFIER);
    }

    @Override
    protected void registerBottomNavigation() {
        bottomNavigationView = findViewById(R.id.bottom_navigation);
        bottomNavigationView.setVisibility(View.GONE);
    }

    @Override
    public void startFormActivity(JSONObject jsonObject) {
        jsonFormUtils.startJsonForm(jsonObject, this);
    }

    @VisibleForTesting
    public EventRegisterActivityContract.Presenter getPresenter() {
        return (EventRegisterActivityContract.Presenter) presenter;
    }

    @Override
    public void saveJsonForm(String json) {
        // do nothing
    }
}
