package org.smartregister.reveal.presenter;


import android.app.Activity;
import android.content.ContentValues;
import android.content.DialogInterface;
import android.content.Intent;

import androidx.localbroadcastmanager.content.LocalBroadcastManager;

import com.google.gson.Gson;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Triple;
import org.joda.time.DateTime;
import org.joda.time.Months;
import org.joda.time.Years;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.family.contract.FamilyOtherMemberContract.Model;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.interactor.FamilyProfileInteractor;
import org.smartregister.family.presenter.BaseFamilyOtherMemberProfileActivityPresenter;
import org.smartregister.family.util.DBConstants;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.interactor.RevealFamilyOtherMemberInteractor;
import org.smartregister.reveal.model.FamilyProfileModel;
import org.smartregister.reveal.util.AlertDialogUtils;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.FamilyJsonFormUtils;
import org.smartregister.reveal.util.TaskUtils;

import java.text.SimpleDateFormat;
import java.util.Date;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.EVENT_TYPE_FIELD;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LAST_NAME;

/**
 * Created by samuelgithengi on 5/31/19.
 */
public class FamilyOtherMemberPresenter extends BaseFamilyOtherMemberProfileActivityPresenter
        implements FamilyOtherMemberProfileContract.Presenter, FamilyProfileContract.InteractorCallBack {


    public static final String OLD_FAMILY_NAME = "oldFamilyName";
    public static final String NEW_FAMILY_NAME = "newFamilyName";
    public static final String UPDATE_FAMILY_NAME = ".UpdateFamilyName";
    public static final String EVENT = "event";
    public static final String CLIENT = "client";
    public static final String DATE_VOIDED = "dateVoided";
    public static final String EVENTS = "events";
    public static final String BIRTH_DATE = "birthdate";
    public static final String FIRST_NAME = "firstName";
    private CommonPersonObjectClient client;

    private FamilyJsonFormUtils familyJsonFormUtils;

    private String familyBaseEntityId;
    private String familyName;

    private FamilyOtherMemberProfileContract.Interactor otherMemberInteractor;
    private org.smartregister.family.contract.FamilyProfileContract.Interactor profileInteractor;
    private FamilyProfileContract.Model profileModel;
    LocalBroadcastManager localBroadcastManager ;

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
        localBroadcastManager = LocalBroadcastManager.getInstance(this.getView().getContext().getApplicationContext());
    }


    @Override
    public void startFormForEdit(CommonPersonObjectClient client) {
        if (StringUtils.isBlank(familyHead)) {
            startFamilyMemberForm(familyName, false);
        } else if (client.getCaseId().equals(familyHead)) {
            startFamilyMemberForm(familyName, true);
        } else {
            otherMemberInteractor.getFamilyHead(this, familyHead);
        }
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
            Intent localIntent = new Intent(UPDATE_FAMILY_NAME);
            localIntent.putExtra(NEW_FAMILY_NAME,familyEventClient.getClient().getLastName());
            Event event = familyEventClient.getEvent();
            String oldFamilyName = event.getObs().stream().filter(obs -> obs.getFieldCode().equals(FamilyConstants.DatabaseKeys.OLD_FAMILY_NAME)).map(obs -> obs.getValue().toString()).findFirst().get();
            localIntent.putExtra(OLD_FAMILY_NAME,oldFamilyName);
            Gson gson = new Gson();

            EventClientRepository eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
            JSONObject familyClient = eventClientRepository.getClientByBaseEntityId(familyBaseEntityId);
            String eventJson  = gson.toJson(familyEventClient.getEvent());
            try {
                familyClient.put(FIRST_NAME,familyEventClient.getClient().getLastName());
                eventClientRepository.addorUpdateClient(familyBaseEntityId,familyClient);
            } catch (JSONException e) {
                e.printStackTrace();
            }
            CommonRepository commonRepository = RevealApplication.getInstance().getContext().commonrepository(FamilyConstants.TABLE_NAME.FAMILY);
            CommonPersonObject familyRegistration = commonRepository.findByBaseEntityId(familyBaseEntityId);
            ContentValues values = new ContentValues();
            values.put(Constants.DatabaseKeys.FIRST_NAME,familyEventClient.getClient().getLastName());
            commonRepository.updateColumn(FamilyConstants.TABLE_NAME.FAMILY,values,familyRegistration.getCaseId());
            String clientJson = familyClient.toString();
            localIntent.putExtra(EVENT,eventJson);
            localIntent.putExtra(CLIENT,clientJson);
            localBroadcastManager.sendBroadcast(localIntent);
        }
        RevealApplication.getInstance().setRefreshMapOnEventSaved(true);

    }

    @Override
    public void onEditMemberDetails() {
        startFormForEdit(client);
    }

    @Override
    public void updateFamilyMember(String jsonString) {
        try {
            getView().showProgressDialog(org.smartregister.family.R.string.saving_dialog_title);

            FamilyEventClient familyEventClient = profileModel.processUpdateMemberRegistration(jsonString, familyBaseEntityId);
            if (familyEventClient == null) {
                return;
            }
            if(Country.NIGERIA.equals(BuildConfig.BUILD_COUNTRY)){
                updateMDADispenseTasksOnAgeChange(familyEventClient);
            }
            profileInteractor.saveRegistration(familyEventClient, jsonString, true, this);
        } catch (Exception e) {
            getView().hideProgressDialog();
            Timber.e(e);
        }
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

    private void updateMDADispenseTasksOnAgeChange(FamilyEventClient familyEventClient) throws Exception {
        final EventClientRepository eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
        JSONObject familyEventClientOriginal = eventClientRepository.getClientByBaseEntityId(familyEventClient.getClient().getBaseEntityId());
        Date updateBirthDate = familyEventClient.getClient().getBirthdate();
        int updateAge = Years.yearsBetween(new DateTime(updateBirthDate.getTime()), DateTime.now()).getYears();
        int updateMonths = Months.monthsBetween(new DateTime(updateBirthDate.getTime()), DateTime.now()).getMonths();
        Date previouslyEnteredBirthDate = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").parse((String) familyEventClientOriginal.get(BIRTH_DATE));
        int previouslyEnteredAge = Years.yearsBetween(new DateTime(previouslyEnteredBirthDate.getTime()), DateTime.now()).getYears();
        int previouslyEnteredMonths = Months.monthsBetween(new DateTime(previouslyEnteredBirthDate.getTime()), DateTime.now()).getMonths();

        if (previouslyEnteredAge < Constants.MDA_MIN_AGE && previouslyEnteredMonths >= Constants.SMC_DISPENSE_MIN_MONTHS) {
                if (!(updateAge < Constants.MDA_MIN_AGE && updateMonths >= Constants.SMC_DISPENSE_MIN_MONTHS)) {
                    TaskRepository taskRepository = RevealApplication.getInstance().getTaskRepository();
                    taskRepository.cancelTasksForEntity(familyEventClient.getClient().getBaseEntityId());
                    taskRepository.archiveTasksForEntity(familyEventClient.getClient().getBaseEntityId());
                    JSONObject eventsByBaseEntityId = eventClientRepository.getEventsByBaseEntityId(familyEventClient.getClient().getBaseEntityId());
                    JSONArray events = eventsByBaseEntityId.optJSONArray(EVENTS);
                    DateTime now = new DateTime();
                    if (events != null) {
                        for (int i = 0; i < events.length(); i++) {
                            try {
                                JSONObject event = events.getJSONObject(i);
                                String eventType = event.getString(EVENT_TYPE_FIELD);
                                if (!(eventType.equals(FamilyConstants.EventType.FAMILY_MEMBER_REGISTRATION) || eventType.equals(FamilyConstants.EventType.UPDATE_FAMILY_MEMBER_REGISTRATION))) {
                                    event.put(DATE_VOIDED, now);
                                    event.put(EventClientRepository.event_column.syncStatus.name(), BaseRepository.TYPE_Unsynced);
                                }

                            } catch (JSONException e) {
                                Timber.e(e);
                            }
                        }
                    }
                    eventClientRepository.batchInsertEvents(events, 0);
                    RevealApplication.getInstance().setSynced(false);
                }
            } else {
                if (updateAge < Constants.MDA_MIN_AGE && updateMonths >= Constants.SMC_DISPENSE_MIN_MONTHS) {
                    TaskUtils.getInstance().generateMDADispenseTask(RevealApplication.getInstance().getContext().applicationContext(), familyEventClient.getClient().getBaseEntityId(), ((FamilyProfileModel) profileModel).getStructureId());
                }
            }
        }
    }
