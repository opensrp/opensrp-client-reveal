package org.smartregister.reveal.sync;

import android.content.Context;
import android.util.Log;

import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.Client;
import org.smartregister.domain.db.Event;
import org.smartregister.domain.db.EventClient;
import org.smartregister.domain.jsonmapping.ClientClassification;
import org.smartregister.repository.BaseRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants.JsonForm;
import org.smartregister.sync.ClientProcessorForJava;

import java.util.List;

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

        if (!eventClients.isEmpty()) {
            for (EventClient eventClient : eventClients) {
                Event event = eventClient.getEvent();
                if (event == null) {
                    return;
                }
                String eventType = event.getEventType();
                if (eventType == null) {
                    continue;
                } else if (eventType.equals(SPRAY_EVENT)) {
                    processSprayEvent(event, clientClassification, localEvents);
                }

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

    private void processSprayEvent(Event event, ClientClassification clientClassification, boolean localEvents) {
        if (event.getDetails() != null && event.getDetails().get(TASK_IDENTIFIER) != null) {
            String taskIdentifier = event.getDetails().get(TASK_IDENTIFIER);
            Task task = RevealApplication.getInstance().getTaskRepository().getTaskByIdentifier(taskIdentifier);
            if (task != null) {
                task.setBusinessStatus(calculateBusinessStatus(event));
                task.setStatus(Task.TaskStatus.COMPLETED);
                if (localEvents) {
                    task.setSyncStatus(BaseRepository.TYPE_Unsynced);
                }
                RevealApplication.getInstance().getTaskRepository().addOrUpdate(task);
            }
            Location structure = RevealApplication.getInstance().getStructureRepository().getLocationById(event.getBaseEntityId());
            if (structure != null) {
                String structureType = event.findObs(null, false, JsonForm.STRUCTURE_TYPE).getValue().toString();
                structure.getProperties().setType(structureType);
                RevealApplication.getInstance().getStructureRepository().addOrUpdate(structure);
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
    }

    public String calculateBusinessStatus(Event event) {
        String sprayStatus = event.findObs(null, false, JsonForm.SPRAY_STATUS).getValue().toString();
        String structureType = event.findObs(null, false, JsonForm.STRUCTURE_TYPE).getValue().toString();
        if (!JsonForm.RESIDENTIAL.equals(structureType)) {
            return NOT_SPRAYABLE;
        } else {
            return sprayStatus;
        }
    }
}
