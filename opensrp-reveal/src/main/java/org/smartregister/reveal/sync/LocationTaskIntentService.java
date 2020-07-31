package org.smartregister.reveal.sync;

import android.app.IntentService;
import android.content.Intent;
import androidx.annotation.Nullable;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;

import org.smartregister.domain.FetchStatus;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.EventClient;
import org.smartregister.job.SyncServiceJob;
import org.smartregister.receiver.SyncStatusBroadcastReceiver;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.job.RevealSyncSettingsServiceJob;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;
import org.smartregister.sync.helper.LocationServiceHelper;
import org.smartregister.sync.helper.PlanIntentServiceHelper;
import org.smartregister.sync.helper.TaskServiceHelper;
import org.smartregister.util.NetworkUtils;
import org.smartregister.util.SyncUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.Action.STRUCTURE_TASK_SYNCED;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY_MEMBER;

public class LocationTaskIntentService extends IntentService {

    private static final String TAG = "LocationTaskIntentService";

    private SyncUtils syncUtils;

    public LocationTaskIntentService() {
        super(TAG);
    }

    @Override
    protected void onHandleIntent(@Nullable Intent intent) {
        if (!NetworkUtils.isNetworkAvailable()) {
            sendSyncStatusBroadcastMessage(FetchStatus.noConnection);
            return;
        }
        if (!syncUtils.verifyAuthorization()) {
            try {
                syncUtils.logoutUser();
            } catch (Exception e) {
                Timber.e(e);
            }
            return;

        }
        sendSyncStatusBroadcastMessage(FetchStatus.fetchStarted);

        doSync();

        (new AppExecutors()).mainThread().execute(new Runnable() {
            @Override
            public void run() {
                RevealSyncSettingsServiceJob.scheduleJobImmediately(RevealSyncSettingsServiceJob.TAG);
            }
        });
    }

    private void sendSyncStatusBroadcastMessage(FetchStatus fetchStatus) {
        Intent intent = new Intent();
        intent.setAction(SyncStatusBroadcastReceiver.ACTION_SYNC_STATUS);
        intent.putExtra(SyncStatusBroadcastReceiver.EXTRA_FETCH_STATUS, fetchStatus);
        sendBroadcast(intent);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        syncUtils = new SyncUtils(getBaseContext());
        return super.onStartCommand(intent, flags, startId);
    }


    private void doSync() {
        sendSyncStatusBroadcastMessage(FetchStatus.fetchStarted);
        LocationServiceHelper locationServiceHelper = new LocationServiceHelper(
                RevealApplication.getInstance().getLocationRepository(),
                RevealApplication.getInstance().getLocationTagRepository(),
                RevealApplication.getInstance().getStructureRepository());
        TaskServiceHelper taskServiceHelper = TaskServiceHelper.getInstance();
        PlanIntentServiceHelper planServiceHelper = PlanIntentServiceHelper.getInstance();


        List<Location> syncedStructures = locationServiceHelper.fetchLocationsStructures();

        sendSyncStatusBroadcastMessage(FetchStatus.fetchStarted);
        planServiceHelper.syncPlans();

        sendSyncStatusBroadcastMessage(FetchStatus.fetchStarted);
        List<Task> synchedTasks = taskServiceHelper.syncTasks();

        TaskRepository taskRepository = RevealApplication.getInstance().getContext().getTaskRepository();
        taskRepository.updateTaskStructureIdFromStructure(syncedStructures);
        taskRepository.updateTaskStructureIdsFromExistingStructures();
        taskRepository.updateTaskStructureIdsFromExistingClients(FAMILY_MEMBER);

        if (hasChangesInCurrentOperationalArea(syncedStructures, synchedTasks)) {
            Intent intent = new Intent(STRUCTURE_TASK_SYNCED);
            LocalBroadcastManager.getInstance(getApplicationContext()).sendBroadcast(intent);
        }

        clientProcessEvents(extractStructureIds(syncedStructures, synchedTasks));

        if (!org.smartregister.util.Utils.isEmptyCollection(syncedStructures)
                || !org.smartregister.util.Utils.isEmptyCollection(synchedTasks)) {
            doSync();
        }

        new AppExecutors().mainThread().execute(new Runnable() {
            @Override
            public void run() {
                SyncServiceJob.scheduleJobImmediately(SyncServiceJob.TAG);
            }
        });

    }

    /**
     * Checks if there a synched structure or task on the currently opened operational area
     *
     * @param syncedStructures the list of synced structures
     * @param synchedTasks     the list of synced tasks
     * @return true if there is a synched structure or task on the currently opened operational area; otherwise returns false
     */
    private boolean hasChangesInCurrentOperationalArea(List<Location> syncedStructures, List<Task> synchedTasks) {
        Location operationalAreaLocation = Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        String operationalAreaLocationId;
        if (operationalAreaLocation == null) {
            return false;
        } else {
            operationalAreaLocationId = operationalAreaLocation.getId();
        }
        if (syncedStructures != null) {
            for (Location structure : syncedStructures) {
                if (operationalAreaLocationId.equals(structure.getProperties().getParentId())) {
                    return true;
                }
            }
        }
        if (synchedTasks != null) {
            for (Task task : synchedTasks) {
                if (operationalAreaLocationId.equals(task.getGroupIdentifier())) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Extracts a set of Structures ids from syched structures and tasks
     *
     * @param syncedStructures the list of synced structures
     * @param synchedTasks     the list of synced tasks
     * @return a set of baseEntityIds
     */
    private Set<String> extractStructureIds(List<Location> syncedStructures, List<Task> synchedTasks) {
        Set<String> structureIds = new HashSet<>();
        if (!org.smartregister.util.Utils.isEmptyCollection(syncedStructures)) {
            for (Location structure : syncedStructures) {
                structureIds.add(structure.getId());
            }
        }
        if (!org.smartregister.util.Utils.isEmptyCollection(synchedTasks)) {
            for (Task task : synchedTasks) {
                structureIds.add(task.getForEntity());
            }
        }
        return structureIds;
    }

    /**
     * Clients Processes events of a set of structure baseEntityIds that have the task status TYPE_Task_Unprocessed
     *
     * @param syncedStructuresIds the set of structure baseEntityIds to client process
     */
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
