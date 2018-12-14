package org.smartregister.reveal.sync;

import android.app.IntentService;
import android.content.Intent;
import android.support.annotation.Nullable;

import org.smartregister.job.SyncServiceJob;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.sync.helper.LocationTaskServiceHelper;

public class LocationTaskIntentService extends IntentService {

    private static final String TAG = "LocationTaskIntentService";

    public LocationTaskIntentService() {
        super(TAG);
    }


    @Override
    protected void onHandleIntent(@Nullable Intent intent) {
        doSync();

    }

    private void doSync() {


        LocationTaskServiceHelper locationTaskServiceHelper = new LocationTaskServiceHelper(RevealApplication.getInstance().getTaskRepository(), RevealApplication.getInstance().getLocationRepository(), RevealApplication.getInstance().getStructureRepository());

        locationTaskServiceHelper.fetchLocationsStructures();
        locationTaskServiceHelper.syncTasks();

        (new AppExecutors()).mainThread().execute(new Runnable() {
            @Override
            public void run() {
                SyncServiceJob.scheduleJobImmediately(SyncServiceJob.TAG);
            }
        });

    }

}
