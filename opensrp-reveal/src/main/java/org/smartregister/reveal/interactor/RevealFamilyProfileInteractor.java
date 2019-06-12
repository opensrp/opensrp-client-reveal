package org.smartregister.reveal.interactor;

import android.content.Context;
import android.support.annotation.NonNull;
import android.util.Log;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.CoreLibrary;
import org.smartregister.clientandeventmodel.Client;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.family.domain.FamilyMetadata;
import org.smartregister.family.interactor.FamilyProfileInteractor;
import org.smartregister.family.util.DBConstants.KEY;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.FamilyJsonFormUtils;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.sync.ClientProcessorForJava;

import java.util.ArrayList;
import java.util.List;

import static org.smartregister.util.JsonFormUtils.gson;


/**
 * Created by samuelgithengi on 4/15/19.
 */
public class RevealFamilyProfileInteractor extends FamilyProfileInteractor implements FamilyProfileContract.Interactor {

    private TaskUtils taskUtils;

    private AppExecutors appExecutors;
    private FamilyProfileContract.Presenter presenter;
    private EventClientRepository eventClientRepository;
    private FamilyMetadata familyMetadata;
    private RevealClientProcessor clientProcessor;

    public RevealFamilyProfileInteractor(FamilyProfileContract.Presenter presenter) {
        this.presenter = presenter;
        taskUtils = TaskUtils.getInstance();
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        eventClientRepository = CoreLibrary.getInstance().context().getEventClientRepository();
        familyMetadata = RevealApplication.getInstance().getMetadata();
        clientProcessor = (RevealClientProcessor) RevealApplication.getInstance().getClientProcessor();
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
    public void updateFamilyMemberSurname(@NonNull Client family, Event event, @NonNull String oldSurname) {
        appExecutors.diskIO().execute(() -> {
            CommonRepository commonRepository = getCommonRepository(familyMetadata.familyMemberRegister.tableName);
            JSONArray familyMembers = new JSONArray();
            List<Event> updateSurnameEvents = new ArrayList<>();
            List<String> formSubmissionIds = new ArrayList<>();
            for (CommonPersonObject commonPersonObject : commonRepository.findByRelational_IDs(family.getBaseEntityId())) {
                String lastName = commonPersonObject.getColumnmaps().get(KEY.LAST_NAME);
                if (oldSurname.equals(lastName)) {//surname same as the edited family name
                    JSONObject client = eventClientRepository.getClientByBaseEntityId(commonPersonObject.getCaseId());
                    try {
                        client.put("lastName", family.getFirstName());
                        familyMembers.put(client);
                        Event updateEvent = FamilyJsonFormUtils.createUpdateMemberSurnameEvent(commonPersonObject.getCaseId(), event);
                        updateSurnameEvents.add(updateEvent);
                        formSubmissionIds.add(updateEvent.getFormSubmissionId());

                    } catch (JSONException e) {
                        Log.e(TAG, "Error updating Family Surname", e);
                    }
                }
            }

            //update the client documents
            eventClientRepository.batchInsertClients(familyMembers);
            //generate the events for updating the members surname
            try {
                eventClientRepository.batchInsertEvents(new JSONArray(gson.toJson(updateSurnameEvents)), 0);
            } catch (JSONException e) {
                Log.e(TAG, "Error Saving Update Member Event", e);
            }

            //Client Process
            clientProcessor.processClient(eventClientRepository.fetchEventClients(formSubmissionIds), true);

            appExecutors.mainThread().execute(() -> {
                presenter.onMembersUpdated();
            });
        });
    }
}
