package org.smartregister.reveal.view;

import android.content.Intent;
import android.view.View;

import org.json.JSONObject;
import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.EventRegisterFragment;
import org.smartregister.reveal.presenter.BaseRegisterPresenter;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.view.fragment.BaseRegisterFragment;

import java.util.Collections;
import java.util.List;

/**
 * Created by samuelgithengi on 7/30/20.
 */
public class EventRegisterActivity extends BaseRevealRegisterActivity {

    private RevealJsonFormUtils jsonFormUtils;

    @Override
    protected void initializePresenter() {
        presenter = new BaseRegisterPresenter(this);
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
}
