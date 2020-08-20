package org.smartregister.reveal.presenter;

import android.content.Context;
import android.content.DialogInterface;

import androidx.core.util.Pair;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.AllConstants;
import org.smartregister.clientandeventmodel.Obs;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Client;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.presenter.BaseFamilyProfilePresenter;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.dao.ReportDao;
import org.smartregister.reveal.dao.StructureDao;
import org.smartregister.reveal.dao.TaskDetailsDao;
import org.smartregister.reveal.interactor.RevealFamilyOtherMemberInteractor;
import org.smartregister.reveal.interactor.RevealFamilyProfileInteractor;
import org.smartregister.reveal.model.FamilyProfileModel;
import org.smartregister.reveal.model.MDAOutCome;
import org.smartregister.reveal.util.AlertDialogUtils;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.FamilyConstants.DatabaseKeys;
import org.smartregister.reveal.util.FamilyConstants.JSON_FORM;
import org.smartregister.reveal.util.FamilyJsonFormUtils;
import org.smartregister.reveal.util.NativeFormProcessor;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.reveal.util.Utils;
import org.smartregister.sync.helper.ECSyncHelper;
import org.smartregister.util.CallableInteractor;
import org.smartregister.util.CallableInteractorCallBack;
import org.smartregister.util.GenericInteractor;
import org.smartregister.util.JsonFormUtils;

import java.util.Calendar;
import java.util.UUID;
import java.util.concurrent.Callable;

import timber.log.Timber;

import static org.smartregister.family.util.Constants.INTENT_KEY.BASE_ENTITY_ID;
import static org.smartregister.family.util.JsonFormUtils.RELATIONSHIPS;
import static org.smartregister.reveal.application.RevealApplication.getInstance;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY;

/**
 * Created by samuelgithengi on 4/10/19.
 */
public class FamilyProfilePresenter extends BaseFamilyProfilePresenter implements FamilyProfileContract.Presenter, FamilyOtherMemberProfileContract.BasePresenter {
    private AppExecutors appExecutors;
    private SQLiteDatabase database;
    private String structureId;
    private PreferencesUtil preferencesUtil;

    private FamilyJsonFormUtils familyJsonFormUtils;

    private FamilyOtherMemberProfileContract.Interactor otherMemberInteractor;

    private TaskUtils taskUtils = TaskUtils.getInstance();

    private CallableInteractor _interactor;


    public FamilyProfilePresenter(FamilyProfileContract.View view, FamilyProfileContract.Model model, String familyBaseEntityId, String familyHead, String primaryCaregiver, String familyName) {
        super(view, model, familyBaseEntityId, familyHead, primaryCaregiver, familyName);
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        database = RevealApplication.getInstance().getRepository().getReadableDatabase();
        preferencesUtil = PreferencesUtil.getInstance();
        getStructureId(familyBaseEntityId);
        setInteractor(new RevealFamilyProfileInteractor(this));
        try {
            familyJsonFormUtils = new FamilyJsonFormUtils(getView().getApplicationContext());
        } catch (Exception e) {
            Timber.e(e, "error Initializing FamilyJsonFormUtils ");
        }
        otherMemberInteractor = new RevealFamilyOtherMemberInteractor();
    }

    @Override
    public void refreshProfileTopSection(CommonPersonObjectClient client) {
        super.refreshProfileTopSection(client);
        getView().setProfileDetailOne(preferencesUtil.getCurrentOperationalArea());
        getView().setProfileDetailTwo(preferencesUtil.getCurrentDistrict());
    }


    private void getStructureId(String familyId) {
        appExecutors.diskIO().execute(() -> {
            Cursor cursor = null;
            try {
                cursor = database.rawQuery(String.format("SELECT DISTINCT %s FROM %S WHERE %s = ?",
                        STRUCTURE_ID, FAMILY, BASE_ENTITY_ID), new String[]{familyId});
                if (cursor.moveToNext()) {
                    structureId = cursor.getString(0);
                }
            } catch (Exception e) {
                Timber.e(e, "Error getting residence for" + familyId);
            } finally {
                if (cursor != null)
                    cursor.close();
            }
            //if (structureId != null) {
            appExecutors.mainThread().execute(() -> {
                getModel().setStructureId(structureId);
                getView().setStructureId(structureId);
            });
            //}
        });
    }

    private FamilyProfileModel getModel() {
        return (FamilyProfileModel) model;
    }

    public String getStructureId() {
        return structureId;
    }

    @Override
    public FamilyProfileContract.View getView() {
        return (FamilyProfileContract.View) super.getView();
    }

    @Override
    public void onRegistrationSaved(boolean editMode, boolean isSaved, FamilyEventClient eventClient) {
        if (!editMode && isSaved && Utils.isFocusInvestigationOrMDA()) {
            getInteractor().generateTasks(getView().getApplicationContext(),
                    eventClient.getEvent().getBaseEntityId(), structureId);
            return;
        } else if (editMode && isSaved) {
            for (Obs obs : eventClient.getEvent().getObs()) {
                if (obs.getFieldCode().equals(DatabaseKeys.OLD_FAMILY_NAME)) {
                    String oldSurname = obs.getValue().toString();
                    if (!eventClient.getClient().getFirstName().equals(oldSurname)) {  //family name was changed
                        getInteractor().updateFamilyMemberName(eventClient.getClient(), eventClient.getEvent(), oldSurname);
                        getView().updateFamilyName(eventClient.getClient().getFirstName());
                        return;
                    }
                }
            }
        }
        super.onRegistrationSaved(editMode, isSaved, eventClient);
    }

    @Override
    public void onTasksGenerated() {
        super.onRegistrationSaved(false, true, null);
        getView().refreshTasks(structureId);

    }

    @Override
    public void onMembersUpdated() {
        onTasksGenerated();
    }

    @Override
    public void onAddFamilyMember() {
        if (getModel().getFamilyHeadPersonObject() == null) {
            otherMemberInteractor.getFamilyHead(this, familyHead);
        } else {
            openAddMemberForm();
        }
    }

    @Override
    public void onArchiveFamilyClicked() {
        AlertDialogUtils.displayNotificationWithCallback(getView().getContext(),
                R.string.confirm_archive_family, R.string.confirm_archive_family_message, R.string.confirm, R.string.cancel, (dialog, buttonClicked) -> {
                    if (buttonClicked == DialogInterface.BUTTON_POSITIVE) {
                        archiveFamily();
                    }
                    dialog.dismiss();
                });
    }

    @Override
    public void onArchiveFamilyCompleted(boolean isSuccessfulSaved, Task task) {
        getView().hideProgressDialog();
        if (!isSuccessfulSaved) {
            AlertDialogUtils.displayNotification(getView().getContext(), R.string.archive_family,
                    R.string.archive_family_failed, familyName);
        } else {
            RevealApplication.getInstance().setRefreshMapOnEventSaved(true);
            getView().returnToMapView(structureId, task);
        }
    }

    private void archiveFamily() {
        getView().showProgressDialog(org.smartregister.family.R.string.saving_dialog_title);
        getInteractor().archiveFamily(familyBaseEntityId, structureId);
    }

    private FamilyProfileContract.Interactor getInteractor() {
        return (FamilyProfileContract.Interactor) interactor;
    }

    @Override
    public void startFormForEdit(CommonPersonObjectClient client) {
        String formName;
        if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
            formName = JSON_FORM.THAILAND_FAMILY_UPDATE;
        } else if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
            formName = JSON_FORM.ZAMBIA_FAMILY_UPDATE;
        } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
            formName = JSON_FORM.REFAPP_FAMILY_UPDATE;
        } else {
            formName = JSON_FORM.FAMILY_UPDATE;
        }
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditFormString(formName,
                client, RevealApplication.getInstance().getMetadata().familyRegister.updateEventType);
        try {
            getView().startFormActivity(form);

        } catch (Exception e) {
            Timber.e(e);
        }
    }

    public ECSyncHelper getSyncHelper() {
        return ECSyncHelper.getInstance(getInstance().getContext().applicationContext());
    }

    public void mergeAndSaveClient(Client client) throws JSONException {
        JSONObject updatedClientJson = new JSONObject(JsonFormUtils.gson.toJson(client));

        JSONObject originalClientJsonObject = getSyncHelper().getClient(client.getBaseEntityId());

        JSONObject mergedJson = JsonFormUtils.merge(originalClientJsonObject, updatedClientJson);

        //retain existing relationships, relationships are deleted on @Link org.smartregister.util.JsonFormUtils.createBaseClient
        JSONObject relationships = mergedJson.optJSONObject(RELATIONSHIPS);
        if ((relationships == null || relationships.length() == 0) && originalClientJsonObject != null) {
            mergedJson.put(RELATIONSHIPS, originalClientJsonObject.optJSONObject(RELATIONSHIPS));
        }

        getSyncHelper().addClient(client.getBaseEntityId(), mergedJson);
    }

    public void updateFamilyMemberHead(JSONObject jsonObject, String familyEntityId, String familyName) {
        CallableInteractor myInteractor = getCallableInteractor();

        Callable<Void> callable = () -> {

            NativeFormProcessor processor = NativeFormProcessor.createInstance(jsonObject);
            Location operationalArea = processor.getCurrentOperationalArea();

            // save family
            String nsac = processor.getFieldValue("nSAC");

            // read the family head and update the family
            EventClientRepository eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
            Client familyClient = eventClientRepository.fetchClientByBaseEntityId(familyEntityId);
            familyClient.addAttribute("nsac", nsac);
            mergeAndSaveClient(familyClient);

            // create update family event
            NativeFormProcessor familyProcessor = NativeFormProcessor.createInstanceFromAsset(org.smartregister.reveal.util.Constants.JsonForm.NTD_COMMUNITY_FAMILY_HEAD_UPDATE);
            familyProcessor
                    .withBindType(FAMILY)
                    .withEntityId(familyEntityId)
                    .withEncounterType(FamilyConstants.EventType.UPDATE_FAMILY_REGISTRATION)
                    .tagLocationData(operationalArea)
                    .tagEventMetadata()

                    .hasClient(true)
                    .mergeAndSaveClient()
                    .saveEvent()

                    .clientProcessForm();

            // save the family member

            String age = processor.getFieldValue("age");
            String same_as_fam_name = processor.getFieldValue("same_as_fam_name");
            String entityId = jsonObject.getString(Constants.Properties.BASE_ENTITY_ID);

            processor
                    .withEntityId(entityId)
                    .withBindType(FamilyConstants.TABLE_NAME.FAMILY_MEMBER)
                    .withEncounterType(FamilyConstants.EventType.UPDATE_FAMILY_MEMBER_REGISTRATION)
                    .tagLocationData(operationalArea)
                    .tagEventMetadata()

                    // create and save client
                    .hasClient(true)
                    .saveClient(client -> {
                        client.setBirthdateApprox(true);
                        if (StringUtils.isNotBlank(age)) {
                            Calendar calendar = Calendar.getInstance();
                            calendar.add(Calendar.YEAR, -1 * Integer.parseInt(age));
                            client.setBirthdate(calendar.getTime());
                        }
                        if (same_as_fam_name.contains("same_as_fam_name"))
                            client.setLastName(familyName);
                    })
                    .mergeAndSaveClient()
                    .saveEvent()
                    .clientProcessForm();


            PreferencesUtil prefsUtil = PreferencesUtil.getInstance();
            ReportDao reportDao = ReportDao.getInstance();
            MDAOutCome mdaOutCome = reportDao.calculateFamilyMDA(familyEntityId, prefsUtil.getCurrentPlanId(), prefsUtil.getCurrentOperationalAreaId());

            MDAOutCome.MDAOutComeStatus mdaOutComeStatus = mdaOutCome.getStatus();

            String status;
            switch (mdaOutComeStatus) {
                case PARTIAL:
                    status = Constants.BusinessStatus.VISITED_PARTIALLY_TREATED;
                    break;
                case POSITIVE:
                    status = Constants.BusinessStatus.COMPLETE;
                    break;
                default:
                    status = Constants.BusinessStatus.VISITED_NOT_TREATED;
            }

            String structureId = StructureDao.getInstance().getStructureIDFromFamilyID(familyEntityId);

            if(StringUtils.isNotBlank(structureId)){
                TaskUtils taskUtils = TaskUtils.getInstance();
                taskUtils.updateTaskStatus(
                        structureId,
                        Constants.Intervention.STRUCTURE_VISITED,
                        status,
                        Task.TaskStatus.COMPLETED
                );
            }

            return null;
        };

        getView().setLoadingState(true);
        myInteractor.execute(callable, new CallableInteractorCallBack<Void>() {
            @Override
            public void onResult(Void aVoid) {
                FamilyProfileContract.View view = getView();
                if (view != null) {
                    view.setLoadingState(false);
                    view.refreshViews(structureId);
                }
            }

            @Override
            public void onError(Exception ex) {
                FamilyProfileContract.View view = getView();
                if (view == null) return;
                view.onError(ex);
                view.setLoadingState(false);
            }
        });
    }

    public void updateFamilyMember(JSONObject jsonObject, String familyEntityId, String familyName) {
        CallableInteractor myInteractor = getCallableInteractor();

        Callable<Void> callable = () -> {

            // save the child
            NativeFormProcessor processor = NativeFormProcessor.createInstance(jsonObject);

            // prevent form submission if consent was not granted
            String consentRelationship = processor.getFieldValue("consentRelationship");
            if (consentRelationship.contains("not_eligible"))
                throw new IllegalAccessException("Attempting to save a child with no consent");


            Location operationalArea = processor.getCurrentOperationalArea();
            String entityId = UUID.randomUUID().toString();

            String age = processor.getFieldValue("age");
            String same_as_fam_name = processor.getFieldValue("same_as_fam_name");

            // update metadata
            processor.withBindType(FamilyConstants.TABLE_NAME.FAMILY_MEMBER)
                    .withEncounterType(FamilyConstants.EventType.FAMILY_MEMBER_REGISTRATION)
                    .withEntityId(entityId)
                    .tagLocationData(operationalArea)
                    .tagEventMetadata()

                    // create and save client
                    .hasClient(true)
                    .saveClient(client -> {
                        client.addRelationship("family", familyEntityId);
                        client.setBirthdateApprox(true);

                        if (same_as_fam_name.contains("same_as_fam_name"))
                            client.setLastName(familyName);

                        if (StringUtils.isNotBlank(age)) {
                            Calendar calendar = Calendar.getInstance();
                            calendar.add(Calendar.YEAR, -1 * Integer.parseInt(age));

                            client.setBirthdate(calendar.getTime());
                        }

                        client.addAttribute("residence", structureId);

                        if (operationalArea != null)
                            client.addAttribute("residential_area", operationalArea.getId());
                    })
                    // create and save event to db
                    .saveEvent()

                    // execute client processing
                    .clientProcessForm();

            return null;
        };

        getView().setLoadingState(true);
        myInteractor.execute(callable, new CallableInteractorCallBack<Void>() {
            @Override
            public void onResult(Void aVoid) {
                FamilyProfileContract.View view = getView();
                if (view != null) {
                    view.setLoadingState(false);
                    view.refreshViews(structureId);
                }
            }

            @Override
            public void onError(Exception ex) {
                FamilyProfileContract.View view = getView();
                if (view == null) return;
                view.onError(ex);
                view.setLoadingState(false);
            }
        });
    }

    public void saveFamilyMember(Context context, JSONObject jsonObject, String familyEntityId, String familyName) {
        CallableInteractor myInteractor = getCallableInteractor();

        Callable<Void> callable = () -> {

            // save the child
            NativeFormProcessor processor = NativeFormProcessor.createInstance(jsonObject);

            // prevent form submission if consent was not granted
            String consentRelationship = processor.getFieldValue("consentRelationship");
            if (consentRelationship.contains("not_eligible"))
                throw new IllegalAccessException("Attempting to save a child with no consent");

            Location operationalArea = processor.getCurrentOperationalArea();
            String entityId = UUID.randomUUID().toString();

            String areaId = StructureDao.getInstance().getStructureIDFromFamilyID(familyEntityId);
            if (StringUtils.isBlank(areaId))
                areaId = operationalArea.getId();

            String age = processor.getFieldValue("age");
            String same_as_fam_name = processor.getFieldValue("same_as_fam_name");

            // generate mda task
            Task registrationTask = taskUtils.generateTask(RevealApplication.getInstance().getContext().applicationContext(),
                    areaId, areaId, Constants.BusinessStatus.FAMILY_MEMBER_REGISTERED, Task.TaskStatus.COMPLETED,
                    Constants.Intervention.FAMILY_MEMBER_REGISTRATION,
                    R.string.register_family_members);

            // update metadata
            String finalAreaId = areaId;
            processor.withBindType(FamilyConstants.TABLE_NAME.FAMILY_MEMBER)
                    .withEncounterType(FamilyConstants.EventType.FAMILY_MEMBER_REGISTRATION)
                    .withEntityId(entityId)
                    .tagTaskDetails(registrationTask)
                    .tagLocationData(operationalArea)
                    .tagEventMetadata()

                    // create and save client
                    .hasClient(true)
                    .saveClient(client -> {
                        client.addRelationship("family", familyEntityId);
                        client.setBirthdateApprox(true);

                        if (same_as_fam_name.contains("same_as_fam_name"))
                            client.setLastName(familyName);

                        if (StringUtils.isNotBlank(age)) {
                            Calendar calendar = Calendar.getInstance();
                            calendar.add(Calendar.YEAR, -1 * Integer.parseInt(age));

                            client.setBirthdate(calendar.getTime());
                        }

                        client.addAttribute("residence", finalAreaId);

                        if (operationalArea != null)
                            client.addAttribute("residential_area", operationalArea.getId());
                    })
                    // create and save event to db
                    .saveEvent()

                    // execute client processing
                    .clientProcessForm()

                    // close client id
                    .closeRegistrationID(Constants.DatabaseKeys.UNIQUE_ID);


            // generate mda task
            // calculate task
            if (StringUtils.isNotBlank(age)) {
                Integer current_age = Integer.parseInt(age);

                if (current_age >= 5 && current_age <= 15)
                    taskUtils.generateTask(context, entityId, areaId, Constants.BusinessStatus.NOT_VISITED, Constants.Intervention.NTD_MDA_DISPENSE,
                            R.string.mass_drug_administration);

                PreferencesUtil prefsUtil = PreferencesUtil.getInstance();
                StructureDao structureDao = StructureDao.getInstance();
                Pair<String, String> result = structureDao.getFamilyIdAndStructureIdByMemberId(entityId);

                ReportDao reportDao = ReportDao.getInstance();
                MDAOutCome mdaOutCome = reportDao.calculateFamilyMDA(result.first, prefsUtil.getCurrentPlanId(), prefsUtil.getCurrentOperationalAreaId());

                MDAOutCome.MDAOutComeStatus mdaOutComeStatus = mdaOutCome.getStatus();

                String status;
                switch (mdaOutComeStatus) {
                    case PARTIAL:
                        status = Constants.BusinessStatus.VISITED_PARTIALLY_TREATED;
                        break;
                    case POSITIVE:
                        status = Constants.BusinessStatus.COMPLETE;
                        break;
                    default:
                        status = Constants.BusinessStatus.VISITED_NOT_TREATED;
                }

                if (StringUtils.isNotBlank(result.second)) {
                    TaskUtils taskUtils = TaskUtils.getInstance();
                    taskUtils.updateTaskStatus(
                            result.second,
                            Constants.Intervention.STRUCTURE_VISITED,
                            status,
                            Task.TaskStatus.COMPLETED
                    );
                } else {
                    // floating family MDA
                    Task floatingRegistration = TaskDetailsDao.getInstance().getCurrentTask(result.first, Constants.Intervention.FLOATING_FAMILY_REGISTRATION);
                    floatingRegistration.setBusinessStatus(status);
                    floatingRegistration.setStatus(Task.TaskStatus.COMPLETED);

                    if (BaseRepository.TYPE_Synced.equals(floatingRegistration.getSyncStatus())) {
                        floatingRegistration.setSyncStatus(BaseRepository.TYPE_Unsynced);
                    }
                    floatingRegistration.setLastModified(new DateTime());
                    RevealApplication.getInstance().getTaskRepository().addOrUpdate(floatingRegistration);
                }
            }

            return null;
        };

        getView().setLoadingState(true);
        myInteractor.execute(callable, new CallableInteractorCallBack<Void>() {
            @Override
            public void onResult(Void aVoid) {
                FamilyProfileContract.View view = getView();
                if (view != null) {
                    view.setLoadingState(false);
                    view.refreshViews(structureId);
                }
            }

            @Override
            public void onError(Exception ex) {
                FamilyProfileContract.View view = getView();
                if (view == null) return;
                view.onError(ex);
                view.setLoadingState(false);
            }
        });
    }


    public CallableInteractor getCallableInteractor() {
        if (_interactor == null)
            _interactor = new GenericInteractor();

        return _interactor;
    }

    @Override
    public void onFetchFamilyHead(CommonPersonObject familyHeadPersonObject) {
        getModel().setFamilyHeadPersonObject(familyHeadPersonObject);
        openAddMemberForm();
    }

    @Override
    public void onArchiveMemberCompleted(boolean isSuccessful) {
        //not used
    }

    private void openAddMemberForm() {
        try {
            startForm(RevealApplication.getInstance().getMetadata().familyMemberRegister.formName, null, null, RevealApplication.getInstance().getContext().allSharedPreferences().getPreference(AllConstants.CURRENT_LOCATION_ID));
        } catch (Exception e) {
            Timber.e(e, "Error opening add member form");
        }
    }
}
