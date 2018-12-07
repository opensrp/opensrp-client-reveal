package org.smartregister.reveal.sync;

import android.content.Context;
import android.util.Log;

import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.Client;
import org.smartregister.domain.db.Event;
import org.smartregister.domain.db.EventClient;
import org.smartregister.domain.jsonmapping.ClientClassification;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.sync.ClientProcessorForJava;
import org.smartregister.util.Utils;

import java.util.List;

import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;

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
    public void processClient(List<EventClient> eventClients) {

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
                } else if (eventType.equals("Spray")) {
                    processSprayEvent(event);
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

    public void processSprayEvent(Event event) {
        if (event.getDetails() != null && event.getDetails().get(TASK_IDENTIFIER) != null) {
            String taskIdentifier = event.getDetails().get(TASK_IDENTIFIER);
            Task task = RevealApplication.getInstance().getTaskRepository().getTaskByIdentifier(taskIdentifier);
            if (task != null) {
                task.setBusinessStatus(event.findObs(null, false, "sprayStatus").getValue().toString());
                task.setStatus(Task.TaskStatus.COMPLETED);
            }
           /*  TODO uncomment after clarification
           Location structure = RevealApplication.getInstance().getStructureRepository().getLocationById(event.getBaseEntityId());
           if (structure != null) {
                structure.getProperties().setName();
                structure.getProperties().setType();
                structure.getProperties().setVersion();
            }*/
        }
    }
}
