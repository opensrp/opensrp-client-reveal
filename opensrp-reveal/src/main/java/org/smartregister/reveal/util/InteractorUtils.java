package org.smartregister.reveal.util;

import android.database.Cursor;

import org.joda.time.DateTime;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.clientandeventmodel.Obs;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.domain.db.Client;
import org.smartregister.domain.db.EventClient;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.FamilyConstants.EventType;

import java.util.Collections;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.SPRAYED_STRUCTURES;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.util.JsonFormUtils.gson;

public class InteractorUtils {

    private TaskRepository taskRepository;

    private EventClientRepository eventClientRepository;

    private RevealClientProcessor clientProcessor;

    public InteractorUtils(TaskRepository taskRepository, EventClientRepository eventClientRepository, RevealClientProcessor clientProcessor) {
        this.taskRepository = taskRepository;
        this.eventClientRepository = eventClientRepository;
        this.clientProcessor = clientProcessor;
    }


    public InteractorUtils() {
    }

    public CommonPersonObject fetchSprayDetails(String interventionType, String structureId, EventClientRepository eventClientRepository, CommonRepository commonRepository) {
        CommonPersonObject commonPersonObject = null;
        if (IRS.equals(interventionType)) {
            Cursor cursor = null;
            try {
                cursor = eventClientRepository.getWritableDatabase().rawQuery(
                        String.format("select s.*, id as _id from %s s where %s = ?", SPRAYED_STRUCTURES, Constants.DatabaseKeys.BASE_ENTITY_ID), new String[]{structureId});
                if (cursor.moveToFirst()) {
                    commonPersonObject = commonRepository.getCommonPersonObjectFromCursor(cursor);
                }
            } catch (Exception e) {
                Timber.e(e);
            } finally {
                if (cursor != null) {
                    cursor.close();
                }
            }
        }
        return commonPersonObject;
    }


    public boolean archiveClient(String baseEntityId, boolean isFamily) {
        taskRepository.cancelTasksForEntity(baseEntityId);
        taskRepository.archiveTasksForEntity(baseEntityId);
        JSONObject eventsByBaseEntityId = eventClientRepository.getEventsByBaseEntityId(baseEntityId);
        JSONArray events = eventsByBaseEntityId.optJSONArray("events");
        JSONObject clientJsonObject = eventsByBaseEntityId.optJSONObject("client");
        DateTime now = new DateTime();
        if (events != null) {
            for (int i = 0; i < events.length(); i++) {
                try {
                    JSONObject event = events.getJSONObject(i);
                    event.put("dateVoided", now);
                    event.put(EventClientRepository.event_column.syncStatus.name(), BaseRepository.TYPE_Unsynced);
                } catch (JSONException e) {
                    Timber.e(e);
                }
            }
        }

        boolean saved;
        try {
            eventClientRepository.batchInsertEvents(events, 0);
            clientJsonObject.put("dateVoided", now);
            clientJsonObject.put(EventClientRepository.client_column.syncStatus.name(), BaseRepository.TYPE_Unsynced);
            clientJsonObject.getJSONObject("attributes").put("dateRemoved", now);
            eventClientRepository.addorUpdateClient(baseEntityId, clientJsonObject);

            Event archiveEvent = FamilyJsonFormUtils.createFamilyEvent(baseEntityId, Utils.getCurrentLocationId(),
                    null, isFamily ? EventType.ARCHIVE_FAMILY : EventType.ARCHIVE_FAMILY_MEMBER);
            archiveEvent.addObs(new Obs().withValue(now).withFieldCode("dateRemoved").withFieldType("formsubmissionField"));

            JSONObject eventJson = new JSONObject(gson.toJson(archiveEvent));
            eventJson.put(EventClientRepository.event_column.syncStatus.name(), BaseRepository.TYPE_Unsynced);
            eventClientRepository.addEvent(baseEntityId, eventJson);

            clientProcessor.processClient(Collections.singletonList(new EventClient(
                    gson.fromJson(eventJson.toString(), org.smartregister.domain.db.Event.class),
                    gson.fromJson(clientJsonObject.toString(), Client.class))), true);
            saved = true;

        } catch (JSONException e) {
            Timber.e(e);
            saved = false;
        }
        return saved;
    }
}
