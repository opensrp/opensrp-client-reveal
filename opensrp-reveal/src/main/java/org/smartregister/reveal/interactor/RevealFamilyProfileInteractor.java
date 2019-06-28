package org.smartregister.reveal.interactor;

import android.content.Context;
import android.support.annotation.NonNull;
import android.util.Log;

import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.CoreLibrary;
import org.smartregister.clientandeventmodel.Client;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.family.domain.FamilyMetadata;
import org.smartregister.family.interactor.FamilyProfileInteractor;
import org.smartregister.family.util.DBConstants.KEY;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.FamilyJsonFormUtils;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.sync.ClientProcessorForJava;
import org.smartregister.util.JsonFormUtils;

import java.util.ArrayList;
import java.util.List;

import static org.smartregister.repository.EventClientRepository.client_column.syncStatus;


/**
 * Created by samuelgithengi on 4/15/19.
 */
public class RevealFamilyProfileInteractor extends FamilyProfileInteractor implements FamilyProfileContract.Interactor {

    private TaskUtils taskUtils;

    private AppExecutors appExecutors;
    private FamilyProfileContract.Presenter presenter;
    private EventClientRepository eventClientRepository;
    private RevealClientProcessor clientProcessor;
    private CommonRepository commonRepository;

    public RevealFamilyProfileInteractor(FamilyProfileContract.Presenter presenter) {
        this.presenter = presenter;
        taskUtils = TaskUtils.getInstance();
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        eventClientRepository = CoreLibrary.getInstance().context().getEventClientRepository();
        FamilyMetadata familyMetadata = RevealApplication.getInstance().getMetadata();
        clientProcessor = (RevealClientProcessor) RevealApplication.getInstance().getClientProcessor();
        commonRepository = RevealApplication.getInstance().getContext().commonrepository(familyMetadata.familyMemberRegister.tableName);
    }

    @Override
    public ClientProcessorForJava getClientProcessorForJava() {
        return RevealClientProcessor.getInstance(RevealApplication.getInstance().getApplicationContext());
    }

    @Override
    public void generateTasks(Context applicationContext, String baseEntityId) {
        appExecutors.diskIO().execute(() -> {
            taskUtils.generateBloodScreeningTask(applicationContext,
                    baseEntityId);
            appExecutors.mainThread().execute(() -> {
                presenter.onTasksGenerated();
            });
        });
    }

    @Override
    public void updateFamilyMemberName(@NonNull Client family, Event event, @NonNull String oldFamilyName) {
        appExecutors.diskIO().execute(() -> {
            JSONArray familyMembers = new JSONArray();
            JSONArray updateSurnameEvents = new JSONArray();
            List<String> formSubmissionIds = new ArrayList<>();
            for (CommonPersonObject commonPersonObject : commonRepository.findByRelational_IDs(family.getBaseEntityId())) {
                String firstName = commonPersonObject.getColumnmaps().get(KEY.FIRST_NAME);
                if (oldFamilyName.equalsIgnoreCase(firstName)) {//name same as the edited family name
                    JSONObject client = eventClientRepository.getClientByBaseEntityId(commonPersonObject.getCaseId());
                    try {
                        client.put("firstName", family.getFirstName());
                        client.put(syncStatus.name(), BaseRepository.TYPE_Unsynced);
                        familyMembers.put(client);
                        Event updateEvent = FamilyJsonFormUtils.createUpdateMemberNameEvent(commonPersonObject.getCaseId(), event);
                        JSONObject eventJson = new JSONObject(JsonFormUtils.gson.toJson(updateEvent));
                        eventJson.put(syncStatus.name(), BaseRepository.TYPE_Unsynced);
                        updateSurnameEvents.put(eventJson);
                        formSubmissionIds.add(updateEvent.getFormSubmissionId());

                    } catch (Exception e) {
                        Log.e(TAG, "Error updating Family Surname", e);
                    }
                }
            }

            //update the client documents
            eventClientRepository.batchInsertClients(familyMembers);
            //generate the events for updating the members surname
            eventClientRepository.batchInsertEvents(updateSurnameEvents, 0);

            //Client Process
            clientProcessor.processClient(eventClientRepository.fetchEventClients(formSubmissionIds), true);

            appExecutors.mainThread().execute(() -> {
                presenter.onMembersUpdated();
            });
        });
    }

}
