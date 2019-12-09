package org.smartregister.reveal.interactor;


import org.joda.time.DateTime;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.domain.Task;
import org.smartregister.family.interactor.FamilyOtherMemberProfileInteractor;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.EventClientRepository.client_column;
import org.smartregister.repository.EventClientRepository.event_column;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract.Interactor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.FamilyJsonFormUtils;
import org.smartregister.reveal.util.Utils;
import org.smartregister.util.JsonFormUtils;

import timber.log.Timber;

import static org.smartregister.repository.EventClientRepository.client_column.syncStatus;
import static org.smartregister.reveal.application.RevealApplication.getInstance;

public class RevealFamilyOtherMemberInteractor extends FamilyOtherMemberProfileInteractor implements Interactor {

    private CommonRepository commonRepository;

    private AppExecutors appExecutors;

    private TaskRepository taskRepository;

    private EventClientRepository eventClientRepository;

    public RevealFamilyOtherMemberInteractor() {
        commonRepository = getInstance().getContext().commonrepository(getInstance().getMetadata().familyMemberRegister.tableName);
        appExecutors = getInstance().getAppExecutors();
        taskRepository = getInstance().getTaskRepository();
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
    public void archiveFamilyMember(CommonPersonObjectClient client) {

        appExecutors.diskIO().execute(() -> {
            taskRepository.cancelTasksByEntityAndStatus(client.getCaseId(), Task.TaskStatus.READY);
            JSONObject eventsByBaseEntityId = eventClientRepository.getEventsByBaseEntityId(client.getCaseId());
            JSONArray events = eventsByBaseEntityId.optJSONArray("events");
            JSONObject clientJsonObject = eventsByBaseEntityId.optJSONObject("client");
            if (events != null) {
                for (int i = 0; i < events.length(); i++) {
                    try {
                        JSONObject event = events.getJSONObject(i);
                        event.put("dateVoided", new DateTime());
                        event.remove(event_column.syncStatus.name());

                    } catch (JSONException e) {
                        Timber.e(e);
                    }
                }
            }
            eventClientRepository.batchInsertClients(events);
            try {
                clientJsonObject.put("dateVoided", new DateTime());
                clientJsonObject.remove(client_column.syncStatus.name());
                eventClientRepository.addorUpdateClient(client.getCaseId(), clientJsonObject);
            } catch (JSONException e) {
                Timber.e(e);
            }


            Event archiveEvent = FamilyJsonFormUtils.createFamilyEvent(client.getCaseId(), Utils.getCurrentLocationId(), null, FamilyConstants.EventType.ARCHIVE_FAMILY_MEMBER);
            try {
                JSONObject eventJson = new JSONObject(JsonFormUtils.gson.toJson(archiveEvent));
                eventJson.put(event_column.syncStatus.name(), BaseRepository.TYPE_Unsynced);
                eventClientRepository.addEvent(client.getCaseId(), eventJson);
            } catch (JSONException e) {
                Timber.e(e);
            }

        });
    }
}
