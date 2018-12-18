package org.smartregister.reveal.sync;

import android.content.Intent;

import org.smartregister.domain.FetchStatus;
import org.smartregister.reveal.job.LocationTaskServiceJob;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.sync.intent.CampaignIntentService;
import org.smartregister.util.Utils;

public class RevealCampaignIntentService extends CampaignIntentService {

    @Override
    protected void onHandleIntent(Intent intent) {
        sendBroadcast(Utils.completeSync(FetchStatus.fetchStarted));
        super.onHandleIntent(intent);
        (new AppExecutors()).mainThread().execute(new Runnable() {
            @Override
            public void run() {
                LocationTaskServiceJob.scheduleJobImmediately(LocationTaskServiceJob.TAG);
            }
        });
    }

}
