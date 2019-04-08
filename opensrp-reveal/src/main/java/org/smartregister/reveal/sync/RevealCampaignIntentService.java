package org.smartregister.reveal.sync;

import android.content.Intent;

import org.smartregister.domain.FetchStatus;
import org.smartregister.reveal.job.LocationTaskServiceJob;
import org.smartregister.reveal.job.RevealSyncSettingsServiceJob;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.sync.intent.CampaignIntentService;
import org.smartregister.util.NetworkUtils;
import org.smartregister.util.SyncUtils;
import org.smartregister.util.Utils;

public class RevealCampaignIntentService extends CampaignIntentService {

    private SyncUtils syncUtils;

    @Override
    protected void onHandleIntent(Intent intent) {
        if (!NetworkUtils.isNetworkAvailable()) {
            sendBroadcast(Utils.completeSync(FetchStatus.noConnection));
            return;
        }
        if (!syncUtils.verifyAuthorization()) {
            syncUtils.logoutUser();
            return;

        }
        sendBroadcast(Utils.completeSync(FetchStatus.fetchStarted));
        super.onHandleIntent(intent);
        (new AppExecutors()).mainThread().execute(new Runnable() {
            @Override
            public void run() {
                LocationTaskServiceJob.scheduleJobImmediately(LocationTaskServiceJob.TAG);
                RevealSyncSettingsServiceJob.scheduleJobImmediately(RevealSyncSettingsServiceJob.TAG);
            }
        });
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        syncUtils = new SyncUtils(getBaseContext());
        return super.onStartCommand(intent, flags, startId);
    }
}
