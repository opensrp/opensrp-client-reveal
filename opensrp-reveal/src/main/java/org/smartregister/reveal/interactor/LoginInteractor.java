package org.smartregister.reveal.interactor;

import org.smartregister.job.CampaignServiceJob;
import org.smartregister.job.LocationStructureServiceJob;
import org.smartregister.job.SyncServiceJob;
import org.smartregister.job.SyncTaskServiceJob;
import org.smartregister.login.interactor.BaseLoginInteractor;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.util.TestDataUtils;
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


        CampaignServiceJob.scheduleJob(CampaignServiceJob.TAG, TimeUnit.MINUTES.toMillis(BuildConfig.SYNC_INTERVAL_IN_MINUTES), getFlexValue(BuildConfig
                .SYNC_INTERVAL_IN_MINUTES));


        LocationStructureServiceJob.scheduleJob(LocationStructureServiceJob.TAG, TimeUnit.MINUTES.toMillis(BuildConfig.SYNC_INTERVAL_IN_MINUTES), getFlexValue(BuildConfig
                .SYNC_INTERVAL_IN_MINUTES));

        SyncTaskServiceJob.scheduleJob(SyncTaskServiceJob.TAG, TimeUnit.MINUTES.toMillis(BuildConfig.SYNC_INTERVAL_IN_MINUTES), getFlexValue(BuildConfig
                .SYNC_INTERVAL_IN_MINUTES));

        SyncServiceJob.scheduleJob(SyncServiceJob.TAG, TimeUnit.MINUTES.toMillis(BuildConfig.SYNC_INTERVAL_IN_MINUTES), getFlexValue(BuildConfig
                .SYNC_INTERVAL_IN_MINUTES));

    }

}
