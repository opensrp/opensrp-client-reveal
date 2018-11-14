package org.smartregister.reveal.interactor;

import org.smartregister.login.interactor.BaseLoginInteractor;
import org.smartregister.view.contract.BaseLoginContract;

/**
 * Created by ndegwamartin on 26/06/2018.
 */
public class LoginInteractor extends BaseLoginInteractor implements BaseLoginContract.Interactor {

    public LoginInteractor(BaseLoginContract.Presenter loginPresenter) {
        super(loginPresenter);
    }

    @Override
    protected void scheduleJobs() {
//        schedule jobs
    }

}
