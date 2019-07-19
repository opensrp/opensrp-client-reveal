package org.smartregister.reveal.sync;

import android.content.Context;
import android.content.Intent;
import android.support.v4.content.LocalBroadcastManager;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.Client;
import org.smartregister.domain.db.Event;
import org.smartregister.domain.db.EventClient;
import org.smartregister.domain.db.Obs;
import org.smartregister.domain.jsonmapping.ClientClassification;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.StructureRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.JsonForm;
import org.smartregister.reveal.util.Constants.StructureType;
import org.smartregister.reveal.util.FamilyConstants.EventType;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;
import org.smartregister.sync.ClientProcessorForJava;

import java.util.ArrayList;
import java.util.List;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.Action.STRUCTURE_TASK_SYNCED;
import static org.smartregister.reveal.util.Constants.BEDNET_DISTRIBUTION_EVENT;
import static org.smartregister.reveal.util.Constants.BEHAVIOUR_CHANGE_COMMUNICATION;
import static org.smartregister.reveal.util.Constants.CONFIGURATION.LOCAL_SYNC_DONE;
import static org.smartregister.reveal.util.Constants.LARVAL_DIPPING_EVENT;
import static org.smartregister.reveal.util.Constants.MOSQUITO_COLLECTION_EVENT;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_PARENT;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_UUID;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.REGISTER_STRUCTURE_EVENT;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;
import static org.smartregister.reveal.util.FamilyConstants.EventType.UPDATE_FAMILY_REGISTRATION;
import static org.smartregister.reveal.util.FamilyConstants.RELATIONSHIP.RESIDENCE;

/**
 * Created by samuelgithengi on 12/7/18.
 */
public class RevealClientProcessor extends ClientProcessorForJava {

    private EventClientRepository eventClientRepository;

    private TaskRepository taskRepository;

    private StructureRepository structureRepository;

    public RevealClientProcessor(Context context) {
        super(context);
        eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
        taskRepository = RevealApplication.getInstance().getTaskRepository();
        structureRepository = RevealApplication.getInstance().getStructureRepository();
    }


    public static RevealClientProcessor getInstance(Context context) {
        if (instance == null) {
            instance = new RevealClientProcessor(context);
        }

        return (RevealClientProcessor) instance;
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

        ArrayList<Client> clients = new ArrayList<>();
        Location operationalArea = Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        String operationalAreaLocationId = operationalArea == null ? null : operationalArea.getId();
        boolean hasSyncedEventsInTarget = false;
        if (!eventClients.isEmpty()) {
            String operationalAreaId = null;
            for (EventClient eventClient : eventClients) {
                Event event = eventClient.getEvent();
                if (event == null || event.getEventType() == null) {
                    continue;
                }

                String eventType = event.getEventType();
                if (eventType.equals(SPRAY_EVENT)) {
                    operationalAreaId = processSprayEvent(event, clientClassification, localEvents);
                } else if (eventType.equals(MOSQUITO_COLLECTION_EVENT) || eventType.equals(LARVAL_DIPPING_EVENT)
                        || eventType.equals(BEDNET_DISTRIBUTION_EVENT) ||
                        eventType.equals(BEHAVIOUR_CHANGE_COMMUNICATION)) {
                    operationalAreaId = processEvent(event, clientClassification, localEvents);
                } else if (eventType.equals(REGISTER_STRUCTURE_EVENT)) {
                    operationalAreaId = processRegisterStructureEvent(event, clientClassification);
                } else if (eventType.equals(UPDATE_FAMILY_REGISTRATION)) {
                    processUpdateFamilyRegistrationEvent(event, eventClient.getClient(), clientClassification, localEvents);
                } else {
                    Client client = eventClient.getClient();

                    if (client != null) {
                        clients.add(client);
                        try {
                            if (event.getDetails() != null && event.getDetails().get(TASK_IDENTIFIER) != null) {
                                updateTask(event, localEvents);
                            }
                            processEvent(event, client, clientClassification);
                        } catch (Exception e) {
                            Timber.e(e);
                        }

                    }
                }
                if (!hasSyncedEventsInTarget && operationalAreaLocationId != null &&
                        operationalAreaLocationId.equals(operationalAreaId)) {
                    hasSyncedEventsInTarget = true;
                }
            }
        }

        taskRepository.updateTaskStructureIdFromClient(clients, RESIDENCE);

        if (hasSyncedEventsInTarget) {
            Intent intent = new Intent(STRUCTURE_TASK_SYNCED);
            intent.putExtra(LOCAL_SYNC_DONE, localEvents);
            LocalBroadcastManager.getInstance(getContext()).sendBroadcast(intent);
        }
    }

    private String processRegisterStructureEvent(Event event, ClientClassification clientClassification) {
        try {
            processEvent(event, new Client(event.getBaseEntityId()), clientClassification);
            if (event.getDetails() != null && event.getDetails().get(LOCATION_PARENT) != null) {
                return event.getDetails().get(LOCATION_PARENT);
            }
        } catch (Exception e) {
            Timber.e(e, "Error processing register structure event");
        }
        return null;
    }

    private void processUpdateFamilyRegistrationEvent(Event event, Client client, ClientClassification clientClassification, boolean localEvents) {
        if (!localEvents ) {
            return;
        }
        try {
            Location structure = null;
            if (event.getDetails() != null) {
                structure = structureRepository.getLocationById(event.getDetails().get(LOCATION_UUID));
            }

            if (structure != null && client.getAddresses() != null && !client.getAddresses().isEmpty()) {
                String houseNumber = client.getAddresses().get(0).getAddressField("address2");
                if (StringUtils.isEmpty(houseNumber)) {
                    return;
                } else if (structure.getProperties() != null
                        && !houseNumber.equalsIgnoreCase(structure.getProperties().getName())) {
                    structure.getProperties().setName(houseNumber);
                    structure.setSyncStatus(BaseRepository.TYPE_Created);
                    structureRepository.addOrUpdate(structure);
                }
                processEvent(event, client, clientClassification);
            }
        } catch (Exception e) {
            Timber.e(e, "Error processing update family registration event");
        }
    }

    private String processSprayEvent(Event event, ClientClassification clientClassification, boolean localEvents) {
        String operationalAreaId = null;
        if (event.getDetails() != null && event.getDetails().get(TASK_IDENTIFIER) != null) {
            operationalAreaId = updateTask(event, localEvents);

            Location structure = structureRepository.getLocationById(event.getBaseEntityId());
            if (structure != null) {
                Obs structureType = event.findObs(null, false, JsonForm.STRUCTURE_TYPE);
                if (structureType != null) {
                    structure.getProperties().setType(structureType.getValue().toString());
                    structureRepository.addOrUpdate(structure);
                }
                if (operationalAreaId == null) {
                    operationalAreaId = structure.getProperties().getParentId();
                }
            }

            try {
                Client client = new Client(event.getBaseEntityId());
                processEvent(event, client, clientClassification);
            } catch (Exception e) {
                Timber.e(e, "Error processing spray event");
            }
        } else {
            Timber.w(String.format("Spray Event %s does not have task details", event.getEventId()));
        }
        return operationalAreaId;
    }

    private String processEvent(Event event, ClientClassification clientClassification, boolean localEvents) {
        String operationalAreaId = null;
        if (event.getDetails() != null && event.getDetails().get(TASK_IDENTIFIER) != null) {
            operationalAreaId = updateTask(event, localEvents);
            try {
                Client client = new Client(event.getBaseEntityId());
                processEvent(event, client, clientClassification);
            } catch (Exception e) {
                Timber.e(e, "Error processing spray event");
            }
        }
        return operationalAreaId;
    }

    private String updateTask(Event event, boolean localEvents) {
        String taskIdentifier = event.getDetails().get(TASK_IDENTIFIER);
        Task task = taskRepository.getTaskByIdentifier(taskIdentifier);
        String operationalAreaId = null;
        if (task != null) {
            task.setBusinessStatus(calculateBusinessStatus(event));
            task.setStatus(Task.TaskStatus.COMPLETED);
            // update task sync status to unsynced if it was already synced,
            // ignore if task status is created so that it will be created on server
            if (localEvents && BaseRepository.TYPE_Synced.equals(task.getSyncStatus())) {
                task.setSyncStatus(BaseRepository.TYPE_Unsynced);
            } else if (!localEvents) {
                // for events synced from server and task exists mark events as being fully synced
                eventClientRepository.markEventAsSynced(event.getFormSubmissionId());
            }
            taskRepository.addOrUpdate(task);
            operationalAreaId = task.getGroupIdentifier();
        } else {
            eventClientRepository.markEventAsTaskUnprocessed(event.getFormSubmissionId());
        }
        return operationalAreaId;
    }

    public String calculateBusinessStatus(Event event) {
        if (EventType.FAMILY_REGISTRATION.equals(event.getEventType()) || EventType.FAMILY_MEMBER_REGISTRATION.equals(event.getEventType())) {
            return BusinessStatus.COMPLETE;
        }
        Obs businessStatusObs = event.findObs(null, false, JsonForm.BUSINESS_STATUS);
        if (businessStatusObs != null) {
            return businessStatusObs.getValue().toString();
        } else {
            //supported only for backward compatibility, business status now being calculated on form
            Obs structureType = event.findObs(null, false, JsonForm.STRUCTURE_TYPE);
            if (structureType != null && !StructureType.RESIDENTIAL.equals(structureType.getValue().toString())) {
                return BusinessStatus.NOT_SPRAYABLE;
            } else {
                Obs sprayStatus = event.findObs(null, false, JsonForm.SPRAY_STATUS);
                return sprayStatus == null ? null : sprayStatus.getValue().toString();
            }
        }
    }
}
