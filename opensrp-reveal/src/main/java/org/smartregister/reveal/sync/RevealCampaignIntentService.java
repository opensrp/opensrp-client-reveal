package org.smartregister.reveal.sync;

import android.content.Intent;

import org.smartregister.reveal.job.LocationTaskServiceJob;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.sync.intent.CampaignIntentService;

public class RevealCampaignIntentService extends CampaignIntentService {

    @Override
    protected void onHandleIntent(Intent intent) {
        super.onHandleIntent(intent);

        (new AppExecutors()).mainThread().execute(new Runnable() {
            @Override
            public void run() {
                LocationTaskServiceJob.scheduleJobImmediately(LocationTaskServiceJob.TAG);
            }
        });
    }

}
