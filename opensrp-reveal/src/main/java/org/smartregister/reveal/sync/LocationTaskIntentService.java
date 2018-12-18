package org.smartregister.reveal.sync;

import android.app.IntentService;
import android.content.Intent;
import android.support.annotation.Nullable;

import org.smartregister.domain.FetchStatus;
import org.smartregister.job.SyncServiceJob;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.sync.helper.LocationServiceHelper;
import org.smartregister.sync.helper.TaskServiceHelper;
import org.smartregister.util.Utils;

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

        LocationServiceHelper locationServiceHelper = new LocationServiceHelper(RevealApplication.getInstance().getLocationRepository(), RevealApplication.getInstance().getStructureRepository());

        TaskServiceHelper taskServiceHelper = new TaskServiceHelper(RevealApplication.getInstance().getTaskRepository());

        locationServiceHelper.fetchLocationsStructures();
        taskServiceHelper.syncTasks();

        (new AppExecutors()).mainThread().execute(new Runnable() {
            @Override
            public void run() {
                SyncServiceJob.scheduleJobImmediately(SyncServiceJob.TAG);
            }
        });
    }

}
