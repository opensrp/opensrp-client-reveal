package org.smartregister.reveal.interactor;

import android.content.Context;

import androidx.annotation.NonNull;

import net.sqlcipher.database.SQLiteDatabase;

import org.joda.time.DateTime;
import org.joda.time.Months;
import org.joda.time.Years;
import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.CoreLibrary;
import org.smartregister.clientandeventmodel.Client;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.EventClient;
import org.smartregister.family.domain.FamilyMetadata;
import org.smartregister.family.interactor.FamilyProfileInteractor;
import org.smartregister.family.util.DBConstants.KEY;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.FamilyJsonFormUtils;
import org.smartregister.reveal.util.InteractorUtils;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.reveal.util.Utils;
import org.smartregister.sync.ClientProcessorForJava;
import org.smartregister.util.JsonFormUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import timber.log.Timber;

import static org.smartregister.family.util.DBConstants.KEY.BASE_ENTITY_ID;
import static org.smartregister.family.util.DBConstants.KEY.DATE_REMOVED;
import static org.smartregister.repository.EventClientRepository.client_column.syncStatus;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY_MEMBER;


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
    private InteractorUtils interactorUtils;
    private TaskRepository taskRepository;

    public RevealFamilyProfileInteractor(FamilyProfileContract.Presenter presenter) {
        this.presenter = presenter;
        taskUtils = TaskUtils.getInstance();
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        eventClientRepository = CoreLibrary.getInstance().context().getEventClientRepository();
        FamilyMetadata familyMetadata = RevealApplication.getInstance().getMetadata();
        clientProcessor = (RevealClientProcessor) RevealApplication.getInstance().getClientProcessor();
        commonRepository = RevealApplication.getInstance().getContext().commonrepository(familyMetadata.familyMemberRegister.tableName);
        interactorUtils = new InteractorUtils(RevealApplication.getInstance().getTaskRepository(), eventClientRepository, clientProcessor);
        taskRepository = RevealApplication.getInstance().getTaskRepository();
    }

    @Override
    public ClientProcessorForJava getClientProcessorForJava() {
        return RevealClientProcessor.getInstance(RevealApplication.getInstance().getApplicationContext());
    }

    @Override
    public void generateTasks(Context applicationContext, String baseEntityId, String structureId, Date birthDate) {
        appExecutors.diskIO().execute(() -> {
            if (Utils.isFocusInvestigation()) {
                taskUtils.generateBloodScreeningTask(applicationContext, baseEntityId, structureId);
                RevealApplication.getInstance().setRefreshMapOnEventSaved(true);
            } else if (Utils.isMDA()) {
                int age = Years.yearsBetween(new DateTime(birthDate.getTime()), DateTime.now()).getYears();
                int months =  Months.monthsBetween(new DateTime(birthDate.getTime()),DateTime.now()).getMonths();
                if (age < Constants.MDA_MIN_AGE && months >= Constants.SMC_DISPENSE_MIN_MONTHS) {
                    taskUtils.generateMDADispenseTask(applicationContext, baseEntityId, structureId);
                    RevealApplication.getInstance().setRefreshMapOnEventSaved(true);
                }
            }
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
                String lastName = commonPersonObject.getColumnmaps().get(KEY.LAST_NAME);
                if (oldFamilyName.equalsIgnoreCase(lastName)) {//surname same as the edited family name
                    JSONObject client = eventClientRepository.getClientByBaseEntityId(commonPersonObject.getCaseId());
                    try {
                        client.put("lastName", family.getFirstName());
                        client.put(syncStatus.name(), BaseRepository.TYPE_Unsynced);
                        familyMembers.put(client);
                        FamilyMetadata familyMetadata = RevealApplication.getInstance().getMetadata();
                        Event updateEvent = FamilyJsonFormUtils.createFamilyEvent(commonPersonObject.getCaseId(),
                                event.getLocationId(), event.getDetails(), familyMetadata.familyMemberRegister.updateEventType);
                        JSONObject eventJson = new JSONObject(JsonFormUtils.gson.toJson(updateEvent));
                        eventJson.put(syncStatus.name(), BaseRepository.TYPE_Unsynced);
                        updateSurnameEvents.put(eventJson);
                        formSubmissionIds.add(updateEvent.getFormSubmissionId());

                    } catch (Exception e) {
                        Timber.e(e, "Error updating Family Surname");
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

    @Override
    public void archiveFamily(String familyBaseEntityId, String structureId) {
        appExecutors.diskIO().execute(() -> {
            SQLiteDatabase db = eventClientRepository.getWritableDatabase();
            boolean saved = false;
            Task task = null;
            try {
                db.beginTransaction();
                List<String> familyMembers = commonRepository.findSearchIds(String.format(
                        "SELECT %s FROM %s where %s='%s' AND %s IS NULL",
                        BASE_ENTITY_ID, FAMILY_MEMBER, KEY.RELATIONAL_ID, familyBaseEntityId, DATE_REMOVED));
                interactorUtils.archiveClient(familyBaseEntityId, true);
                for (String baseEntityId : familyMembers) {
                    interactorUtils.archiveClient(baseEntityId, false);
                }
                taskRepository.cancelTasksForEntity(structureId);
                taskRepository.archiveTasksForEntity(structureId);
                task = taskUtils.generateRegisterFamilyTask(RevealApplication.getInstance().getApplicationContext(), structureId);
                db.setTransactionSuccessful();
                saved = true;
            } catch (Exception e) {
                Timber.e(e);
            } finally {
                db.endTransaction();
            }
            boolean finalSaved = saved;
            Task finalTask = task;
            appExecutors.mainThread().execute(() -> {
                presenter.onArchiveFamilyCompleted(finalSaved, finalTask);
            });
        });
    }

    @Override
    protected void processClient(List<EventClient> eventClientList) {
        clientProcessor.processClient(eventClientList, true);
    }

    @Override
    public void getRegistrationEvent(final CommonPersonObjectClient family, String familyHead) {
        // Heads Up
        appExecutors.diskIO().execute(() -> {

            CommonPersonObject personObject = this.getCommonRepository(org.smartregister.family.util.Utils.metadata().familyRegister.tableName).findByBaseEntityId(familyHead);
            final CommonPersonObjectClient pClient = new CommonPersonObjectClient(personObject.getCaseId(), personObject.getDetails(), "");
            pClient.setColumnmaps(personObject.getColumnmaps());

            final JSONObject structureJson = eventClientRepository.getEventsByBaseEntityIdAndEventType(family.getCaseId(), FamilyConstants.EventType.FAMILY_REGISTRATION);
            final org.smartregister.domain.Event structureEvent = eventClientRepository.convert(structureJson, org.smartregister.domain.Event.class);

            appExecutors.mainThread().execute(() -> {
                presenter.onEventFound(structureEvent, pClient);
            });
        });
    }
}
