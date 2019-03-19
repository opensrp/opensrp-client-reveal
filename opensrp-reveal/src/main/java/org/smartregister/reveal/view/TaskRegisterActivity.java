package org.smartregister.reveal.view;

import android.content.Intent;
import android.support.v4.app.Fragment;
import android.view.View;

import org.json.JSONObject;
import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.TaskRegisterFragment;
import org.smartregister.reveal.presenter.TaskRegisterPresenter;
import org.smartregister.view.activity.BaseRegisterActivity;
import org.smartregister.view.fragment.BaseRegisterFragment;

import java.util.Collections;
import java.util.List;

import static org.smartregister.reveal.util.Constants.TaskRegister;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterActivity extends BaseRegisterActivity {

    @Override
    protected void initializePresenter() {
        presenter = new TaskRegisterPresenter();
    }

    @Override
    protected BaseRegisterFragment getRegisterFragment() {
        return new TaskRegisterFragment();
    }

    @Override
    protected Fragment[] getOtherFragments() {
        return new Fragment[0];
    }

    @Override
    public void startFormActivity(String formName, String entityId, String metaData) {
    }

    @Override
    public void startFormActivity(JSONObject jsonObject) {
    }

    @Override
    protected void onActivityResultExtended(int requestCode, int resultCode, Intent data) {
    }

    @Override
    public List<String> getViewIdentifiers() {
        return Collections.singletonList(TaskRegister.VIEW_IDENTIFIER);
    }

    @Override
    public void startRegistration() {//not used on reveal/ adding points done on map
    }

    @Override
    protected void registerBottomNavigation() {
        //not used for task register
        findViewById(R.id.bottom_navigation).setVisibility(View.GONE);
    }
}
