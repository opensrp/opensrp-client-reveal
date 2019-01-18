package org.smartregister.reveal.sync;

import android.app.IntentService;
import android.content.Intent;
import android.support.annotation.Nullable;
import android.support.v4.content.LocalBroadcastManager;
import android.support.v4.util.Pair;

import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.EventClient;
import org.smartregister.job.SyncServiceJob;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;
import org.smartregister.sync.helper.LocationServiceHelper;
import org.smartregister.sync.helper.TaskServiceHelper;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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
        LocationServiceHelper locationServiceHelper = LocationServiceHelper.getInstance();
        TaskServiceHelper taskServiceHelper = TaskServiceHelper.getInstance();

        List<Location> syncedStructures = locationServiceHelper.fetchLocationsStructures();
        List<Task> synchedTasks = taskServiceHelper.syncTasks();

        Pair<Boolean, Set<String>> structureIds = checkChangeInOperationalAreaAndExtractStructureIds(syncedStructures, synchedTasks);

        clientProcessEvents(structureIds.second);

        new AppExecutors().mainThread().execute(new Runnable() {
            @Override
            public void run() {
                SyncServiceJob.scheduleJobImmediately(SyncServiceJob.TAG);
            }
        });

        if (structureIds.first != null && structureIds.first) {
            Intent intent = new Intent(STRUCTURE_TASK_SYNCHED);
            LocalBroadcastManager.getInstance(getApplicationContext()).sendBroadcast(intent);
        }
    }

    private Pair<Boolean, Set<String>> checkChangeInOperationalAreaAndExtractStructureIds(List<Location> syncedStructures, List<Task> synchedTasks) {
        Location operationalAreaLocation = Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        String operationalAreaLocationId = operationalAreaLocation == null ? null : operationalAreaLocation.getId();
        Set<String> structureIds = new HashSet<>();
        boolean hasChangeInOperationalArea = false;
        if (!org.smartregister.util.Utils.isEmptyCollection(syncedStructures)) {
            for (Location structure : syncedStructures) {
                if (!hasChangeInOperationalArea && operationalAreaLocationId != null && operationalAreaLocationId.equals(structure.getProperties().getParentId())) {
                    hasChangeInOperationalArea = true;
                }
                structureIds.add(structure.getId());
            }
        }
        if (!org.smartregister.util.Utils.isEmptyCollection(synchedTasks)) {
            for (Task task : synchedTasks) {
                if (!hasChangeInOperationalArea && operationalAreaLocationId != null && operationalAreaLocationId.equals(task.getGroupIdentifier())) {
                    hasChangeInOperationalArea = true;
                }
                structureIds.add(task.getForEntity());
            }
        }
        return new Pair<>(hasChangeInOperationalArea, structureIds);
    }

    private void clientProcessEvents(Set<String> syncedStructuresIds) {
        if (org.smartregister.util.Utils.isEmptyCollection(syncedStructuresIds))
            return;
        EventClientRepository ecRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
        List<EventClient> eventClients = ecRepository.getEventsByBaseEntityIdsAndSyncStatus(BaseRepository.TYPE_Task_Unprocessed, new ArrayList<>(syncedStructuresIds));
        if (!eventClients.isEmpty()) {
            RevealClientProcessor.getInstance(getApplicationContext()).processClient(eventClients);
        }
    }

}
