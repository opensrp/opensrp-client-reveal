package org.smartregister.reveal.interactor;

import org.smartregister.login.interactor.BaseLoginInteractor;
import org.smartregister.reveal.util.Utils;
import org.smartregister.view.contract.BaseLoginContract;

public class LoginInteractor extends BaseLoginInteractor implements BaseLoginContract.Interactor {

    public LoginInteractor(BaseLoginContract.Presenter loginPresenter) {
        super(loginPresenter);
    }

    @Override
    protected void scheduleJobs() {
        Utils.startImmediateSync();
    }

}
