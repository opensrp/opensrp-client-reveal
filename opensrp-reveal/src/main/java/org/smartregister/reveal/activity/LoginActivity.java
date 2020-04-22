package org.smartregister.reveal.activity;

import android.content.Intent;

import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.presenter.LoginPresenter;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.view.ChildRegisterActivity;
import org.smartregister.reveal.view.ListTasksActivity;
import org.smartregister.task.SaveTeamLocationsTask;
import org.smartregister.view.activity.BaseLoginActivity;
import org.smartregister.view.contract.BaseLoginContract;

public class LoginActivity extends BaseLoginActivity implements BaseLoginContract.View {

    @Override
    protected int getContentView() {
        return R.layout.activity_login;
    }

    @Override
    protected void initializePresenter() {
        mLoginPresenter = new LoginPresenter(this);
    }

    @Override
    public void goToHome(boolean remote) {
        if (remote) {
            org.smartregister.util.Utils.startAsyncTask(new SaveTeamLocationsTask(), null);
        }

        Class<?> startActivity = ListTasksActivity.class;

        if (BuildConfig.BUILD_COUNTRY == Country.NTD_SCHOOL)
            startActivity = ChildRegisterActivity.class;

        Intent intent = new Intent(this, startActivity);
        startActivity(intent);

        finish();

        RevealApplication.getInstance().processServerConfigs();

    }

    @Override
    protected void onResume() {
        super.onResume();
        mLoginPresenter.processViewCustomizations();
        if (!mLoginPresenter.isUserLoggedOut()) {
            goToHome(false);
        }
    }

}
