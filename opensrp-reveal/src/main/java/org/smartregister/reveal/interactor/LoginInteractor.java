package org.smartregister.reveal.interactor;

import org.smartregister.job.PullUniqueIdsServiceJob;
import org.smartregister.job.SyncServiceJob;
import org.smartregister.login.interactor.BaseLoginInteractor;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.job.RevealCampaignServiceJob;
import org.smartregister.reveal.util.Utils;
import org.smartregister.view.contract.BaseLoginContract;

import java.util.concurrent.TimeUnit;

public class LoginInteractor extends BaseLoginInteractor implements BaseLoginContract.Interactor {

    public LoginInteractor(BaseLoginContract.Presenter loginPresenter) {
        super(loginPresenter);
    }

    @Override
    protected void scheduleJobs() {
        Utils.startImmediateSync();

        RevealCampaignServiceJob.scheduleJob(RevealCampaignServiceJob.TAG, TimeUnit.MINUTES.toMillis(
                BuildConfig.SYNC_INTERVAL_IN_MINUTES), getFlexValue(BuildConfig.SYNC_INTERVAL_IN_MINUTES));

        PullUniqueIdsServiceJob.scheduleJob(SyncServiceJob.TAG, TimeUnit.MINUTES.toMillis(
                BuildConfig.PULL_UNIQUE_IDS_MINUTES), getFlexValue(BuildConfig.PULL_UNIQUE_IDS_MINUTES));
    }

}
