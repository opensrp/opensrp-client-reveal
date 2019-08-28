package org.smartregister.reveal.interactor;

import org.smartregister.job.PullUniqueIdsServiceJob;
import org.smartregister.job.SyncServiceJob;
import org.smartregister.login.interactor.BaseLoginInteractor;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.job.LocationTaskServiceJob;
import org.smartregister.reveal.util.Utils;
import org.smartregister.view.contract.BaseLoginContract;

public class LoginInteractor extends BaseLoginInteractor implements BaseLoginContract.Interactor {

    public LoginInteractor(BaseLoginContract.Presenter loginPresenter) {
        super(loginPresenter);
    }

    @Override
    protected void scheduleJobsPeriodically() {
        LocationTaskServiceJob.scheduleJob(LocationTaskServiceJob.TAG,
                BuildConfig.SYNC_INTERVAL_IN_MINUTES, getFlexValue((int) BuildConfig.SYNC_INTERVAL_IN_MINUTES));

        PullUniqueIdsServiceJob.scheduleJob(SyncServiceJob.TAG,
                BuildConfig.PULL_UNIQUE_IDS_MINUTES, getFlexValue((int) BuildConfig.PULL_UNIQUE_IDS_MINUTES));
    }

    @Override
    protected void scheduleJobsImmediately() {
        Utils.startImmediateSync();
    }

}
