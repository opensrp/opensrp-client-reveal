package org.smartregister.reveal.sync;

import android.app.IntentService;
import android.content.Intent;
import android.support.annotation.Nullable;
import android.support.v4.content.LocalBroadcastManager;

import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.job.SyncServiceJob;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.sync.helper.LocationServiceHelper;
import org.smartregister.sync.helper.TaskServiceHelper;

import java.util.List;

import static org.smartregister.reveal.util.Constants.Action.STRUCTURE_TASK_SYNCHED;

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
        String operationalAreaLocationId = org.smartregister.reveal.util.Utils.getCurrentOperationalAreaId();
        LocationServiceHelper locationServiceHelper = LocationServiceHelper.getInstance();

        locationServiceHelper.setTargetParentIdentifier(operationalAreaLocationId);

        TaskServiceHelper taskServiceHelper = TaskServiceHelper.getInstance();
        taskServiceHelper.setTargetGroupIdentifier(operationalAreaLocationId);

        List<Location> syncedStructures = locationServiceHelper.fetchLocationsStructures();
        List<Task> synchedTasks = taskServiceHelper.syncTasks();

        (new AppExecutors()).mainThread().execute(new Runnable() {
            @Override
            public void run() {
                SyncServiceJob.scheduleJobImmediately(SyncServiceJob.TAG);
            }
        });
        if (!syncedStructures.isEmpty() || !synchedTasks.isEmpty()) {
            Intent intent = new Intent(STRUCTURE_TASK_SYNCHED);
            LocalBroadcastManager.getInstance(getApplicationContext()).sendBroadcast(intent);
        }

    }

}
