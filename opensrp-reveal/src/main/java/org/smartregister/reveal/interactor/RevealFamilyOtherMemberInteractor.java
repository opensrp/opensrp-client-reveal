package org.smartregister.reveal.interactor;


import org.joda.time.DateTime;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.domain.db.Client;
import org.smartregister.domain.db.EventClient;
import org.smartregister.family.interactor.FamilyOtherMemberProfileInteractor;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.EventClientRepository.client_column;
import org.smartregister.repository.EventClientRepository.event_column;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract.Interactor;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.FamilyJsonFormUtils;
import org.smartregister.reveal.util.Utils;

import java.util.Collections;

import timber.log.Timber;

import static org.smartregister.reveal.application.RevealApplication.getInstance;
import static org.smartregister.util.JsonFormUtils.gson;

public class RevealFamilyOtherMemberInteractor extends FamilyOtherMemberProfileInteractor implements Interactor {

    private CommonRepository commonRepository;

    private AppExecutors appExecutors;

    private TaskRepository taskRepository;

    private EventClientRepository eventClientRepository;

    private RevealClientProcessor clientProcessor;

    public RevealFamilyOtherMemberInteractor() {
        commonRepository = getInstance().getContext().commonrepository(getInstance().getMetadata().familyMemberRegister.tableName);
        appExecutors = getInstance().getAppExecutors();
        taskRepository = getInstance().getTaskRepository();
        eventClientRepository = getInstance().getContext().getEventClientRepository();
        clientProcessor = (RevealClientProcessor) getInstance().getClientProcessor();
    }

    @Override
    public void getFamilyHead(FamilyOtherMemberProfileContract.BasePresenter presenter, String familyHeadId) {
        appExecutors.diskIO().execute(() -> {
            CommonPersonObject commonPersonObject = commonRepository.findByBaseEntityId(familyHeadId);
            appExecutors.mainThread().execute(() -> {
                presenter.onFetchFamilyHead(commonPersonObject);
            });
        });
    }

    @Override
    public void archiveFamilyMember(FamilyOtherMemberProfileContract.BasePresenter presenter, CommonPersonObjectClient client) {
        appExecutors.diskIO().execute(() -> {
            taskRepository.cancelTasksForEntity(client.getCaseId());
            JSONObject eventsByBaseEntityId = eventClientRepository.getEventsByBaseEntityId(client.getCaseId());
            JSONArray events = eventsByBaseEntityId.optJSONArray("events");
            JSONObject clientJsonObject = eventsByBaseEntityId.optJSONObject("client");
            DateTime now = new DateTime();
            if (events != null) {
                for (int i = 0; i < events.length(); i++) {
                    try {
                        JSONObject event = events.getJSONObject(i);
                        event.put("dateVoided", now);
                        event.put(event_column.syncStatus.name(), BaseRepository.TYPE_Unsynced);
                    } catch (JSONException e) {
                        Timber.e(e);
                    }
                }
            }

            boolean saved;
            try {
                getInstance().getRepository().getWritableDatabase().beginTransaction();
                eventClientRepository.batchInsertEvents(events, 0);
                clientJsonObject.put("dateVoided", now);
                clientJsonObject.put(client_column.syncStatus.name(), BaseRepository.TYPE_Unsynced);
                clientJsonObject.getJSONObject("attributes").put("dateRemoved", now);
                eventClientRepository.addorUpdateClient(client.getCaseId(), clientJsonObject);

                Event archiveEvent = FamilyJsonFormUtils.createFamilyEvent(client.getCaseId(), Utils.getCurrentLocationId(), null, FamilyConstants.EventType.ARCHIVE_FAMILY_MEMBER);

                JSONObject eventJson = new JSONObject(gson.toJson(archiveEvent));
                eventJson.put(event_column.syncStatus.name(), BaseRepository.TYPE_Unsynced);
                eventClientRepository.addEvent(client.getCaseId(), eventJson);

                clientProcessor.processClient(Collections.singletonList(new EventClient(
                        gson.fromJson(eventJson.toString(), org.smartregister.domain.db.Event.class),
                        gson.fromJson(clientJsonObject.toString(), Client.class))), true);
                getInstance().getRepository().getWritableDatabase().setTransactionSuccessful();
                saved = true;
            } catch (JSONException e) {
                Timber.e(e);
                saved = false;
            } finally {
                getInstance().getRepository().getWritableDatabase().endTransaction();
            }
            boolean finalSaved = saved;
            appExecutors.mainThread().execute(() -> {
                presenter.onArchiveMemberCompleted(finalSaved);
            });

        });
    }
}
