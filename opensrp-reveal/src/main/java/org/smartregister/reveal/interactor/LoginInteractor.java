package org.smartregister.reveal.interactor;

import org.smartregister.job.ImageUploadServiceJob;
import org.smartregister.job.PullUniqueIdsServiceJob;
import org.smartregister.job.SyncServiceJob;
import org.smartregister.login.interactor.BaseLoginInteractor;
import org.smartregister.ug.reveal.BuildConfig;
import org.smartregister.view.contract.BaseLoginContract;

import java.util.concurrent.TimeUnit;

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
        SyncServiceJob.scheduleJob(SyncServiceJob.TAG, TimeUnit.MINUTES.toMillis(BuildConfig.DATA_SYNC_DURATION_MINUTES), getFlexValue(BuildConfig
                .DATA_SYNC_DURATION_MINUTES));

        PullUniqueIdsServiceJob.scheduleJob(SyncServiceJob.TAG, TimeUnit.MINUTES.toMillis(BuildConfig.PULL_UNIQUE_IDS_MINUTES), getFlexValue
                (BuildConfig.PULL_UNIQUE_IDS_MINUTES));

        ImageUploadServiceJob.scheduleJob(SyncServiceJob.TAG, TimeUnit.MINUTES.toMillis(BuildConfig.IMAGE_UPLOAD_MINUTES), getFlexValue(BuildConfig
                .IMAGE_UPLOAD_MINUTES));
    }

}
