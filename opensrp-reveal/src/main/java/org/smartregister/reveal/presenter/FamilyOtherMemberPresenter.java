package org.smartregister.reveal.presenter;


import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;

import androidx.core.util.Pair;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Triple;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.family.contract.FamilyOtherMemberContract.Model;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.interactor.FamilyProfileInteractor;
import org.smartregister.family.presenter.BaseFamilyOtherMemberProfileActivityPresenter;
import org.smartregister.family.util.DBConstants;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.dao.ClientDao;
import org.smartregister.reveal.dao.FamilyMemberDao;
import org.smartregister.reveal.dao.ReportDao;
import org.smartregister.reveal.dao.StructureDao;
import org.smartregister.reveal.interactor.RevealFamilyOtherMemberInteractor;
import org.smartregister.reveal.model.FamilyProfileModel;
import org.smartregister.reveal.model.MDAOutCome;
import org.smartregister.reveal.util.AlertDialogUtils;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.FamilyJsonFormUtils;
import org.smartregister.reveal.util.NativeFormProcessor;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.util.CallableInteractor;
import org.smartregister.util.CallableInteractorCallBack;
import org.smartregister.util.GenericInteractor;
import org.smartregister.util.Utils;

import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.LAST_NAME;
import static org.smartregister.reveal.util.Constants.JsonForm.ENCOUNTER_TYPE;
import static org.smartregister.reveal.util.FamilyConstants.EventType.UPDATE_FAMILY_HEAD_MEMBER_REGISTRATION;
import static org.smartregister.reveal.util.FamilyConstants.EventType.UPDATE_FAMILY_MEMBER_REGISTRATION;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY_MEMBER;
import static org.smartregister.reveal.util.FamilyJsonFormUtils.getFormValue;

/**
 * Created by samuelgithengi on 5/31/19.
 */
public class FamilyOtherMemberPresenter extends BaseFamilyOtherMemberProfileActivityPresenter
        implements FamilyOtherMemberProfileContract.Presenter, FamilyProfileContract.InteractorCallBack {


    private CommonPersonObjectClient client;

    private FamilyJsonFormUtils familyJsonFormUtils;

    private String familyBaseEntityId;
    private String familyName;

    private FamilyOtherMemberProfileContract.Interactor otherMemberInteractor;
    private org.smartregister.family.contract.FamilyProfileContract.Interactor profileInteractor;
    private FamilyProfileContract.Model profileModel;

    private CallableInteractor _interactor;

    public FamilyOtherMemberPresenter(FamilyOtherMemberProfileContract.View view, Model model,
                                      String viewConfigurationIdentifier, String familyBaseEntityId, String baseEntityId,
                                      String familyHead, String primaryCaregiver, String villageTown, String familyName) {
        super(view, model, viewConfigurationIdentifier, baseEntityId, familyHead, primaryCaregiver, villageTown);
        this.familyBaseEntityId = familyBaseEntityId;
        this.familyName = familyName;
        this.otherMemberInteractor = new RevealFamilyOtherMemberInteractor();
        this.profileInteractor = new FamilyProfileInteractor();
        this.profileModel = new FamilyProfileModel(familyName);
        try {
            familyJsonFormUtils = new FamilyJsonFormUtils(view.getContext());
        } catch (Exception e) {
            Timber.e("error starting FamilyJsonFormUtils");
        }
    }


    @Override
    public void startFormForEdit(CommonPersonObjectClient client) {
        if (BuildConfig.BUILD_COUNTRY.equals(Country.NTD_COMMUNITY)) {
            startNTDMemberForm(client);
        } else if (StringUtils.isBlank(familyHead)) {
            startFamilyMemberForm(familyName, false);
        } else if (client.getCaseId().equals(familyHead)) {
            startFamilyMemberForm(familyName, true);
        } else {
            otherMemberInteractor.getFamilyHead(this, familyHead);
        }
    }

    public String readAssetContents(Context context, String path) {
        return Utils.readAssetContents(context, path);
    }

    private JSONObject readEditMemberForm(CommonPersonObjectClient client) throws JSONException {
        String jsonForm = readAssetContents(RevealApplication.getInstance().getContext().applicationContext(), Constants.JsonForm.NTD_COMMUNITY_FAMILY_HEAD_REGISTER);
        JSONObject jsonObject = new JSONObject(jsonForm);
        jsonObject.put(Constants.Properties.BASE_ENTITY_ID, client.getCaseId());
        jsonObject.put(ENCOUNTER_TYPE, UPDATE_FAMILY_HEAD_MEMBER_REGISTRATION);

        ClientDao clientDao = ClientDao.getInstance();

        String clientString = clientDao.getClient(client.getCaseId());
        JSONObject clientJson = new JSONObject(clientString);

        // get key for values
        Map<String, Object> values = new HashMap<>();
        values.put("unique_id", getFormValue(clientJson.getJSONObject("identifiers"), "opensrp_id"));

        values.put("noConsent", getFormValue(clientJson.getJSONObject("attributes"), "no_consent"));

        values.put("firstName", getFormValue(clientJson, "firstName"));
        values.put("surname", getFormValue(clientJson, "lastName"));

        values.put("gender", getFormValue(clientJson, "gender"));
        values.put("age", FamilyJsonFormUtils.getAge((String) getFormValue(clientJson, "birthdate")));
        values.put("personPhonenumber", getFormValue(clientJson.getJSONObject("attributes"), "person_phone_number"));
        values.put("nSAC", getFormValue(clientJson.getJSONObject("attributes"), "nsac"));
        values.put("nameGrantingconsent", getFormValue(clientJson.getJSONObject("attributes"), "name_granting_consent"));

        NativeFormProcessor.createInstance(jsonObject)
                .populateValues(values);

        return jsonObject;
    }

    private JSONObject readEditChildForm(CommonPersonObjectClient client) throws JSONException {
        String jsonForm = readAssetContents(RevealApplication.getInstance().getContext().applicationContext(), Constants.JsonForm.NTD_COMMUNITY_FAMILY_MEMBER_REGISTER);
        JSONObject jsonObject = new JSONObject(jsonForm);
        jsonObject.put(Constants.Properties.BASE_ENTITY_ID, client.getCaseId());
        jsonObject.put(ENCOUNTER_TYPE, UPDATE_FAMILY_MEMBER_REGISTRATION);

        ClientDao clientDao = ClientDao.getInstance();

        String clientString = clientDao.getClient(client.getCaseId());
        JSONObject clientJson = new JSONObject(clientString);

        // get key for values
        Map<String, Object> values = new HashMap<>();
        values.put("unique_id", getFormValue(clientJson.getJSONObject("identifiers"), "opensrp_id"));

        values.put("consentRelationship", getFormValue(clientJson.getJSONObject("attributes"), "relationship"));
        values.put("consentRelationshipOther", getFormValue(clientJson.getJSONObject("attributes"), "consent_relationship_other"));
        values.put("noConsent", getFormValue(clientJson.getJSONObject("attributes"), "no_consent"));

        values.put("firstName", getFormValue(clientJson, "firstName"));
        values.put("surname", getFormValue(clientJson, "lastName"));

        values.put("gender", getFormValue(clientJson, "gender"));
        values.put("age", FamilyJsonFormUtils.getAge((String) getFormValue(clientJson, "birthdate")));
        values.put("personEnrollment", getFormValue(clientJson.getJSONObject("attributes"), "person_enrollment"));

        NativeFormProcessor.createInstance(jsonObject)
                .populateValues(values);

        return jsonObject;
    }

    private void startNTDMemberForm(CommonPersonObjectClient client) {
        CallableInteractor myInteractor = getCallableInteractor();
        Callable<JSONObject> callable = () -> {

            FamilyMemberDao familyMemberDao = FamilyMemberDao.getInstance();

            // check if the family member is a
            return familyMemberDao.isFamilyHead(client.entityId()) ? readEditMemberForm(client) : readEditChildForm(client);
        };


        myInteractor.execute(callable, new CallableInteractorCallBack<JSONObject>() {
            @Override
            public void onResult(JSONObject jsonObject) {
                if (jsonObject != null)
                    getView().startFormActivity(jsonObject);
            }

            @Override
            public void onError(Exception ex) {
                Timber.e(ex);
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
        startFamilyMemberForm(familyHeadPersonObject.getColumnmaps().get(LAST_NAME), false);
    }

    @Override
    public void onArchiveMemberCompleted(boolean isSuccessful) {
        getView().hideProgressDialog();
        if (!isSuccessful) {
            AlertDialogUtils.displayNotification(getView().getContext(), R.string.archive_member,
                    R.string.archive_member_failed, client.getColumnmaps().get(DBConstants.KEY.FIRST_NAME),
                    client.getColumnmaps().get(DBConstants.KEY.LAST_NAME));
        } else {
            ((Activity) getView().getContext()).finish();
        }
    }

    private void startFamilyMemberForm(String familyName, boolean isFamilyHead) {
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditMemberFormString(
                R.string.edit_member_form_title,
                RevealApplication.getInstance().getMetadata().familyMemberRegister.formName,
                client, RevealApplication.getInstance().getMetadata().familyMemberRegister.updateEventType,
                familyName, isFamilyHead);
        getView().startFormActivity(form);
    }

    @Override
    public void refreshProfileTopSection(CommonPersonObjectClient client) {
        super.refreshProfileTopSection(client);
        this.client = client;
    }

    @Override
    public void onUniqueIdFetched(Triple<String, String, String> triple, String entityId) {//not used
    }

    @Override
    public void onNoUniqueId() {//not used
    }


    @Override
    public void onRegistrationSaved(boolean isEditMode, boolean isSaved, FamilyEventClient familyEventClient) {
        if (isEditMode) {
            getView().hideProgressDialog();

            refreshProfileView();

            getView().refreshList();

        }
        RevealApplication.getInstance().setRefreshMapOnEventSaved(true);

    }

    @Override
    public void onEditMemberDetails() {
        startFormForEdit(client);
    }

    public void updateFamilyMemberHead(JSONObject jsonObject, String familyEntityId) {
        CallableInteractor myInteractor = getCallableInteractor();

        Callable<Void> callable = () -> {

            NativeFormProcessor processor = NativeFormProcessor.createInstance(jsonObject);
            Location operationalArea = processor.getCurrentOperationalArea();

            // save family
            String nsac = processor.getFieldValue("nSAC");

            // create update family event
            Map<String, Object> values = new HashMap<>();
            values.put("nSAC", nsac);

            NativeFormProcessor familyProcessor = NativeFormProcessor.createInstanceFromAsset(org.smartregister.reveal.util.Constants.JsonForm.NTD_COMMUNITY_FAMILY_HEAD_UPDATE);
            familyProcessor
                    .withBindType(FAMILY)
                    .withEntityId(familyEntityId)
                    .withEncounterType(FamilyConstants.EventType.UPDATE_FAMILY_REGISTRATION)
                    .populateValues(values)
                    .tagLocationData(operationalArea)
                    .tagEventMetadata()
                    .hasClient(true)
                    .updateAndSaveClient(null)
                    .saveEvent()
                    .clientProcessForm();

            // save the family member
            String age = processor.getFieldValue("age");
            String entityId = jsonObject.getString(Constants.Properties.BASE_ENTITY_ID);

            processor
                    .withEntityId(entityId)
                    .withBindType(FamilyConstants.TABLE_NAME.FAMILY_MEMBER)
                    .withEncounterType(FamilyConstants.EventType.UPDATE_FAMILY_MEMBER_REGISTRATION)
                    .tagLocationData(operationalArea)
                    .tagEventMetadata()

                    // create and save client
                    .hasClient(true)
                    .updateAndSaveClient(client -> {
                        client.setBirthdateApprox(true);
                        if (StringUtils.isNotBlank(age)) {
                            Calendar calendar = Calendar.getInstance();
                            calendar.add(Calendar.YEAR, -1 * Integer.parseInt(age));
                            client.setBirthdate(calendar.getTime());
                        }
                    })
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

            TaskUtils taskUtils = TaskUtils.getInstance();
            if (StringUtils.isNotBlank(structureId)) {
                taskUtils.updateTaskStatus(
                        structureId,
                        Constants.Intervention.STRUCTURE_VISITED,
                        status,
                        Task.TaskStatus.COMPLETED
                );
            } else {
                // floating families task
                taskUtils.updateTaskStatus(
                        familyEntityId,
                        Constants.Intervention.FLOATING_FAMILY_REGISTRATION,
                        status,
                        Task.TaskStatus.COMPLETED
                );
            }

            return null;
        };

        myInteractor.execute(callable, new CallableInteractorCallBack<Void>() {
            @Override
            public void onResult(Void aVoid) {
                onRegistrationSaved(true, true, null);
            }

            @Override
            public void onError(Exception ex) {
                Timber.e(ex);
            }
        });
    }

    @Override
    public void updateFamilyMember(String jsonString) {
        try {
            getView().showProgressDialog(org.smartregister.family.R.string.saving_dialog_title);

            if (BuildConfig.BUILD_COUNTRY.equals(Country.NTD_COMMUNITY)) {
                updateNTDFamilyMember(jsonString);
                return;
            }

            FamilyEventClient familyEventClient = profileModel.processUpdateMemberRegistration(jsonString, familyBaseEntityId);
            if (familyEventClient == null) {
                return;
            }

            profileInteractor.saveRegistration(familyEventClient, jsonString, true, this);
        } catch (Exception e) {
            getView().hideProgressDialog();
            Timber.e(e);
        }
    }

    private void updateNTDFamilyMember(String jsonString) {
        CallableInteractor myInteractor = getCallableInteractor();

        Callable<Void> callable = () -> {

            JSONObject jsonObject = new JSONObject(jsonString);

            NativeFormProcessor processor = NativeFormProcessor.createInstance(jsonObject);
            Location operationalArea = processor.getCurrentOperationalArea();
            String entityId = jsonObject.getString(Constants.Properties.BASE_ENTITY_ID);


            String age = processor.getFieldValue("age");

            // update metadata
            processor.withBindType(FAMILY_MEMBER)
                    .withEncounterType(UPDATE_FAMILY_MEMBER_REGISTRATION)
                    .withEntityId(entityId)
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
                    })
                    .mergeAndSaveClient()

                    // create and save event to db
                    .saveEvent()

                    // execute client processing
                    .clientProcessForm();

            TaskUtils taskUtils = TaskUtils.getInstance();
            if (StringUtils.isNotBlank(age)) {
                Integer current_age = Integer.parseInt(age);
                if (current_age >= 5 && current_age <= 15) {
                    // create a task if not exists
                    Set<Task> tasks = taskUtils.getTasksByEntityAndCode(entityId, Constants.Intervention.NTD_MDA_DISPENSE);
                    // create a new task for missing tasks
                    if (tasks.size() < 1) {

                        Pair<String, String> familyIdAndStructureId = StructureDao.getInstance().getFamilyIdAndStructureIdByMemberId(entityId);

                        taskUtils.generateTask(
                                RevealApplication.getInstance().getContext().applicationContext(),
                                entityId, familyIdAndStructureId.second, Constants.BusinessStatus.NOT_VISITED,
                                Constants.Intervention.NTD_MDA_DISPENSE,
                                R.string.mass_drug_administration
                        );
                    }
                } else {
                    // nuke the task
                    taskUtils
                            .updateTaskStatus(entityId, Constants.Intervention.NTD_MDA_DISPENSE, Constants.BusinessStatus.VOIDED, Task.TaskStatus.CANCELLED);
                }
            }

            return null;
        };

        myInteractor.execute(callable, new CallableInteractorCallBack<Void>() {
            @Override
            public void onResult(Void aVoid) {
                onRegistrationSaved(true, true, null);
            }

            @Override
            public void onError(Exception ex) {
                Timber.e(ex);
            }
        });
    }

    @Override
    public void onArchiveFamilyMember() {
        AlertDialogUtils.displayNotificationWithCallback(getView().getContext(), R.string.archive_member, R.string.confirm_archive_member, R.string.confirm, R.string.cancel, (dialog, buttonClicked) -> {
            if (buttonClicked == DialogInterface.BUTTON_POSITIVE) {
                archiveFamilyMember();
            }
            dialog.dismiss();
        });
    }

    private void archiveFamilyMember() {
        getView().showProgressDialog(org.smartregister.family.R.string.saving_dialog_title);
        otherMemberInteractor.archiveFamilyMember(this, client);
    }

    @Override
    protected FamilyOtherMemberProfileContract.View getView() {
        return (FamilyOtherMemberProfileContract.View) super.getView();
    }

    public void setStructureId(String structureId) {
        ((FamilyProfileModel) profileModel).setStructureId(structureId);
    }
}
