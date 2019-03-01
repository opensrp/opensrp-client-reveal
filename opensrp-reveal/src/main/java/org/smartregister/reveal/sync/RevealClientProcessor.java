package org.smartregister.reveal.sync;

import android.content.Context;
import android.content.Intent;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;

import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.Client;
import org.smartregister.domain.db.Event;
import org.smartregister.domain.db.EventClient;
import org.smartregister.domain.jsonmapping.ClientClassification;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants.JsonForm;
import org.smartregister.reveal.util.Constants.StructureType;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;
import org.smartregister.sync.ClientProcessorForJava;

import java.util.List;

import static org.smartregister.reveal.util.Constants.Action.STRUCTURE_TASK_SYNCHED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_SPRAYABLE;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;

/**
 * Created by samuelgithengi on 12/7/18.
 */
public class RevealClientProcessor extends ClientProcessorForJava {


    private static final String TAG = RevealClientProcessor.class.getCanonicalName();
    private static RevealClientProcessor instance;

    public RevealClientProcessor(Context context) {
        super(context);
    }


    public static RevealClientProcessor getInstance(Context context) {
        if (instance == null) {
            instance = new RevealClientProcessor(context);
        }

        return instance;
    }

    @Override
    public synchronized void processClient(List<EventClient> eventClientList) {
        processClient(eventClientList, false);
    }

    public void processClient(List<EventClient> eventClients, boolean localEvents) {

        ClientClassification clientClassification = assetJsonToJava("ec_client_classification.json", ClientClassification.class);

        if (clientClassification == null) {
            return;
        }
        Location operationalArea = Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        String operationalAreaLocationId = operationalArea == null ? null : operationalArea.getId();
        boolean hasSynchedEventsInTarget = false;

        if (!eventClients.isEmpty()) {
            for (EventClient eventClient : eventClients) {
                Event event = eventClient.getEvent();
                if (event == null || event.getEventType() == null) {
                    continue;
                }
                String eventType = event.getEventType();
                if (eventType.equals(SPRAY_EVENT)) {
                    String operationalAreaId = processSprayEvent(event, clientClassification, localEvents);
                    if (!hasSynchedEventsInTarget && operationalAreaLocationId != null &&
                            operationalAreaLocationId.equals(operationalAreaId)) {
                        hasSynchedEventsInTarget = true;
                    }
                } else {
                    Client client = eventClient.getClient();
                    //iterate through the events
                    if (client != null) {
                        try {
                            processEvent(event, client, clientClassification);
                        } catch (Exception e) {
                            Log.d(TAG, e.getMessage());
                        }

                    }
                }
            }
        }


        if (hasSynchedEventsInTarget) {
            Intent intent = new Intent(STRUCTURE_TASK_SYNCHED);
            LocalBroadcastManager.getInstance(getContext()).sendBroadcast(intent);
        }

    }

    private String processSprayEvent(Event event, ClientClassification clientClassification, boolean localEvents) {
        String operationalAreaId = null;
        if (event.getDetails() != null && event.getDetails().get(TASK_IDENTIFIER) != null) {
            String taskIdentifier = event.getDetails().get(TASK_IDENTIFIER);
            Task task = RevealApplication.getInstance().getTaskRepository().getTaskByIdentifier(taskIdentifier);
            EventClientRepository eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
            if (task != null) {
                task.setBusinessStatus(calculateBusinessStatus(event));
                task.setStatus(Task.TaskStatus.COMPLETED);
                //update task sync status to unsynced if it was already synced, ignore if task status is created so that it will be created on server
                if (localEvents && BaseRepository.TYPE_Synced.equals(task.getSyncStatus())) {
                    //update sync status so that updated task status can be pushed to server
                    task.setSyncStatus(BaseRepository.TYPE_Unsynced);
                } else if (!localEvents) {
                    //for events synced from server and task exists mark events as being fully synced
                    eventClientRepository.markEventAsSynced(event.getFormSubmissionId());
                }
                RevealApplication.getInstance().getTaskRepository().addOrUpdate(task);
                operationalAreaId = task.getGroupIdentifier();
            } else {
                eventClientRepository.markEventAsTaskUnprocessed(event.getFormSubmissionId());
            }
            Location structure = RevealApplication.getInstance().getStructureRepository().getLocationById(event.getBaseEntityId());
            if (structure != null) {
                String structureType = event.findObs(null, false, JsonForm.STRUCTURE_TYPE).getValue().toString();
                structure.getProperties().setType(structureType);
                RevealApplication.getInstance().getStructureRepository().addOrUpdate(structure);
                if (operationalAreaId == null) {
                    operationalAreaId = structure.getProperties().getParentId();
                }
            }
            try {
                Client client = new Client(event.getBaseEntityId());
                processEvent(event, client, clientClassification);
            } catch (Exception e) {
                Log.e(TAG, "Error processing spray event", e);
            }
        } else {
            Log.w(TAG, String.format("Spray Event %s does not have task details", event.getEventId()));
        }
        return operationalAreaId;
    }

    public String calculateBusinessStatus(Event event) {
        String sprayStatus = event.findObs(null, false, JsonForm.SPRAY_STATUS).getValue().toString();
        String structureType = event.findObs(null, false, JsonForm.STRUCTURE_TYPE).getValue().toString();
        if (!StructureType.RESIDENTIAL.equals(structureType)) {
            return NOT_SPRAYABLE;
        } else {
            return sprayStatus;
        }
    }

    @Override
    public void updateClientDetailsTable(Event event, Client client) {
        //do nothing
    }
}
