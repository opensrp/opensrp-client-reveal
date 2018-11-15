package org.smartregister.reveal.activity;

import android.widget.Toast;

import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.smartregister.reveal.event.ViewConfigurationSyncCompleteEvent;
import org.smartregister.reveal.presenter.LoginPresenter;
import org.smartregister.task.SaveTeamLocationsTask;
import org.smartregister.ug.reveal.R;
import org.smartregister.view.activity.BaseLoginActivity;
import org.smartregister.view.contract.BaseLoginContract;

import static org.smartregister.util.Log.logInfo;

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
        Toast.makeText(this, "Successfully logged in need to implement landing page ", Toast.LENGTH_LONG).show();

//        finish();
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
