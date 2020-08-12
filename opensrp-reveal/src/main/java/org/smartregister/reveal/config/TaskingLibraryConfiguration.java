package org.smartregister.reveal.config;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;

import net.sqlcipher.database.SQLiteDatabase;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.CoreLibrary;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.clientandeventmodel.Obs;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.cursoradapter.SmartRegisterQueryBuilder;
import org.smartregister.domain.Client;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.EventClient;
import org.smartregister.family.fragment.NoMatchDialogFragment;
import org.smartregister.family.util.DBConstants;
import org.smartregister.job.DocumentConfigurationServiceJob;
import org.smartregister.job.PullUniqueIdsServiceJob;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.job.LocationTaskServiceJob;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.task.IndicatorsCalculatorTask;
import org.smartregister.tasking.util.CardDetailsUtil;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.FamilyJsonFormUtils;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.tasking.util.Utils;
import org.smartregister.reveal.view.ListTasksActivity;
import org.smartregister.sync.ClientProcessorForJava;
import org.smartregister.tasking.activity.FilterTasksActivity;
import org.smartregister.tasking.adapter.TaskRegisterAdapter;
import org.smartregister.tasking.contract.BaseContract;
import org.smartregister.tasking.contract.BaseFormFragmentContract;
import org.smartregister.tasking.model.BaseTaskDetails;
import org.smartregister.tasking.model.CardDetails;
import org.smartregister.tasking.model.TaskDetails;
import org.smartregister.tasking.model.TaskFilterParams;
import org.smartregister.tasking.util.InteractorUtils;
import org.smartregister.tasking.util.PreferencesUtil;
import org.smartregister.tasking.viewholder.TaskRegisterViewHolder;
import org.smartregister.util.AppExecutors;
import org.smartregister.util.JsonFormUtils;
import org.smartregister.view.activity.BaseRegisterActivity;
import org.smartregister.view.activity.DrishtiApplication;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import timber.log.Timber;

import static android.app.Activity.RESULT_OK;
import static org.smartregister.family.util.DBConstants.KEY.FIRST_NAME;
import static org.smartregister.family.util.Utils.metadata;
import static org.smartregister.reveal.util.Constants.BEDNET_DISTRIBUTION_EVENT;
import static org.smartregister.reveal.util.Constants.BLOOD_SCREENING_EVENT;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CODE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FAMILY_NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FOR;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LATITUDE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LONGITUDE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NOT_SRAYED_OTHER_REASON;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NOT_SRAYED_REASON;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.REFERENCE_REASON;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.SPRAYED_STRUCTURES;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.SPRAY_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURES_TABLE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_NAME;
import static org.smartregister.reveal.util.Constants.EventType.CASE_CONFIRMATION_EVENT;
import static org.smartregister.reveal.util.Constants.Filter.FILTER_SORT_PARAMS;
import static org.smartregister.reveal.util.Constants.Intervention.BCC;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.Intervention.PAOT;
import static org.smartregister.reveal.util.Constants.JsonForm.ENCOUNTER_TYPE;
import static org.smartregister.reveal.util.Constants.REGISTER_STRUCTURE_EVENT;
import static org.smartregister.reveal.util.FamilyConstants.DatabaseKeys.HOUSE_NUMBER;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY;
import static org.smartregister.util.JsonFormUtils.ENTITY_ID;

/**
 * Created by Ephraim Kigamba - nek.eam@gmail.com on 07-08-2020.
 */
public class TaskingLibraryConfiguration extends org.smartregister.tasking.util.TaskingLibraryConfiguration {


    @NonNull
    @Override
    public Pair<Drawable, String> getActionDrawable(Context context, TaskDetails task) {
        // The assumption is that a register structure task always exists if the structure has
        // atleast one bednet distribution or blood screening task
        boolean familyRegTaskMissingOrFamilyRegComplete = task.isFamilyRegistered() || !task.isFamilyRegTaskExists();
        Drawable actionBg = null;
        String actionText = context.getText(R.string.view_tasks).toString();

        if (Utils.isFocusInvestigation()) {
            if (familyRegTaskMissingOrFamilyRegComplete && task.isBednetDistributed() && task.isBloodScreeningDone()) {
                actionBg = context.getResources().getDrawable(R.drawable.tasks_complete_bg);
                actionText = context.getText(R.string.tasks_complete).toString();
            } else if (familyRegTaskMissingOrFamilyRegComplete && !task.isBednetDistributed() && !task.isBloodScreeningDone()) {
                actionBg = context.getResources().getDrawable(R.drawable.family_registered_bg);
            } else if (familyRegTaskMissingOrFamilyRegComplete && task.isBednetDistributed()) {
                actionBg = context.getResources().getDrawable(R.drawable.bednet_distributed_bg);
            } else if (task.isBloodScreeningDone()) {
                actionBg = context.getResources().getDrawable(R.drawable.blood_screening_complete_bg);
            } else {
                actionBg = context.getResources().getDrawable(R.drawable.no_task_complete_bg);
            }
        } else if (Utils.isMDA()) {
            if (familyRegTaskMissingOrFamilyRegComplete && task.isMdaAdhered()) {
                actionBg = context.getResources().getDrawable(R.drawable.mda_adhered_bg);
                actionText = context.getText(R.string.tasks_complete).toString();
            } else if (familyRegTaskMissingOrFamilyRegComplete && task.isFullyReceived()) {
                actionBg = context.getResources().getDrawable(R.drawable.mda_dispensed_bg);
            } else if (familyRegTaskMissingOrFamilyRegComplete && task.isPartiallyReceived()) {
                actionBg = context.getResources().getDrawable(R.drawable.mda_partially_received_bg);
            } else if (familyRegTaskMissingOrFamilyRegComplete && task.isNoneReceived()) {
                actionBg = context.getResources().getDrawable(R.drawable.mda_none_received_bg);
            } else if (familyRegTaskMissingOrFamilyRegComplete && task.isNotEligible()) {
                actionBg = context.getResources().getDrawable(R.drawable.mda_not_eligible_bg);
            } else if (familyRegTaskMissingOrFamilyRegComplete) {
                actionBg = context.getResources().getDrawable(R.drawable.family_registered_bg);
            } else {
                actionBg = context.getResources().getDrawable(R.drawable.no_task_complete_bg);
            }
        }

        return new Pair<>(actionBg, actionText);
    }

    @Override
    public int getInterventionLabel() {
        String plan = PreferencesUtil.getInstance().getCurrentPlan();
        String interventionType = PreferencesUtil.getInstance().getInterventionTypeForPlan(plan);
        if (interventionType.equals(Constants.Intervention.FI))
            return R.string.focus_investigation;
        else if (interventionType.equals(Constants.Intervention.IRS))
            return R.string.irs;
        else if (interventionType.equals(Constants.Intervention.MDA))
            return R.string.mda;
        else
            return R.string.irs;
    }

    @NonNull
    @Override
    public Float getLocationBuffer() {
        return Float.valueOf(Utils.getGlobalConfig(Constants.CONFIGURATION.LOCATION_BUFFER_RADIUS_IN_METRES, BuildConfig.MY_LOCATION_BUFFER + ""));
    }

    @Override
    public void startImmediateSync() {
        LocationTaskServiceJob.scheduleJobImmediately(LocationTaskServiceJob.TAG);
        PullUniqueIdsServiceJob.scheduleJobImmediately(PullUniqueIdsServiceJob.TAG);
        DocumentConfigurationServiceJob.scheduleJobImmediately(DocumentConfigurationServiceJob.TAG);
    }

    @Override
    public boolean validateFarStructures() {
        return Boolean.valueOf(Utils.getGlobalConfig(Constants.CONFIGURATION.VALIDATE_FAR_STRUCTURES, BuildConfig.VALIDATE_FAR_STRUCTURES + ""));
    }

    @Override
    public int getResolveLocationTimeoutInSeconds() {
        return Integer.valueOf(Utils.getGlobalConfig(Constants.CONFIGURATION.RESOLVE_LOCATION_TIMEOUT_IN_SECONDS, BuildConfig.RESOLVE_LOCATION_TIMEOUT_IN_SECONDS + ""));
    }

    @Override
    public String getAdminPasswordNotNearStructures() {
        return Utils.getGlobalConfig(Constants.CONFIGURATION.ADMIN_PASSWORD_NOT_NEAR_STRUCTURES, BuildConfig.ADMIN_PASSWORD_NOT_NEAR_STRUCTURES);
    }

    @Override
    public boolean isFocusInvestigation() {
        return getInterventionLabel() == R.string.focus_investigation;
    }

    @Override
    public boolean isMDA() {
        return getInterventionLabel() == R.string.mda;
    }

    @Override
    public String getCurrentLocationId() {
        Location currentOperationalArea = Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        return currentOperationalArea == null ? null : currentOperationalArea.getId();
    }

    @Override
    public String getCurrentOperationalAreaId() {
        return PreferencesUtil.getInstance().getCurrentOperationalAreaId();
    }

    @Override
    public Integer getDatabaseVersion() {
        return BuildConfig.DATABASE_VERSION;
    }

    @Override
    public void tagEventTaskDetails(List<Event> events, SQLiteDatabase sqLiteDatabase) {
        TaskUtils.getInstance().tagEventTaskDetails(events, sqLiteDatabase);
    }

    @Override
    public Boolean displayDistanceScale() {
        return Boolean.valueOf(Utils.getGlobalConfig(Constants.CONFIGURATION.DISPLAY_DISTANCE_SCALE, BuildConfig.DISPLAY_DISTANCE_SCALE + ""));
    }

    @Override
    public String getFormName(@NonNull String encounterType, @Nullable String taskCode) {
        String formName = null;
        if (Constants.SPRAY_EVENT.equals(encounterType) || Constants.Intervention.IRS.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.NAMIBIA) {
                formName = Constants.JsonForm.SPRAY_FORM_NAMIBIA;
            } else if (BuildConfig.BUILD_COUNTRY == Country.BOTSWANA) {
                formName = Constants.JsonForm.SPRAY_FORM_BOTSWANA;
            } else if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
                formName = Constants.JsonForm.SPRAY_FORM_ZAMBIA;
            } else if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = Constants.JsonForm.THAILAND_SPRAY_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = Constants.JsonForm.SPRAY_FORM_REFAPP;
            } else {
                formName = Constants.JsonForm.SPRAY_FORM;
            }
        } else if (Constants.MOSQUITO_COLLECTION_EVENT.equals(encounterType)
                || Constants.Intervention.MOSQUITO_COLLECTION.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = Constants.JsonForm.THAILAND_MOSQUITO_COLLECTION_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = Constants.JsonForm.REFAPP_MOSQUITO_COLLECTION_FORM;
            } else {
                formName = Constants.JsonForm.MOSQUITO_COLLECTION_FORM;
            }
        } else if (Constants.BEDNET_DISTRIBUTION_EVENT.equals(encounterType)
                || Constants.Intervention.BEDNET_DISTRIBUTION.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = Constants.JsonForm.THAILAND_BEDNET_DISTRIBUTION_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = Constants.JsonForm.REFAPP_BEDNET_DISTRIBUTION_FORM;
            } else {
                formName = Constants.JsonForm.BEDNET_DISTRIBUTION_FORM;
            }
        } else if (Constants.EventType.CASE_CONFIRMATION_EVENT.equals(encounterType)
                || Constants.Intervention.CASE_CONFIRMATION.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = Constants.JsonForm.THAILAND_CASE_CONFIRMATION_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = Constants.JsonForm.REFAPP_CASE_CONFIRMATION_FORM;
            } else {
                formName = Constants.JsonForm.CASE_CONFIRMATION_FORM;
            }
        } else if (Constants.BLOOD_SCREENING_EVENT.equals(encounterType)
                || Constants.Intervention.BLOOD_SCREENING.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = Constants.JsonForm.THAILAND_BLOOD_SCREENING_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = Constants.JsonForm.REFAPP_BLOOD_SCREENING_FORM;
            } else {
                formName = Constants.JsonForm.BLOOD_SCREENING_FORM;
            }
        } else if (Constants.LARVAL_DIPPING_EVENT.equals(encounterType) || Constants.Intervention.LARVAL_DIPPING.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = Constants.JsonForm.THAILAND_LARVAL_DIPPING_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = Constants.JsonForm.REFAPP_LARVAL_DIPPING_FORM;
            } else {
                formName = Constants.JsonForm.LARVAL_DIPPING_FORM;
            }
        } else if (Constants.BEHAVIOUR_CHANGE_COMMUNICATION.equals(encounterType) || Constants.Intervention.BCC.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = Constants.JsonForm.THAILAND_BEHAVIOUR_CHANGE_COMMUNICATION_FORM;
            } else {
                formName = Constants.JsonForm.BEHAVIOUR_CHANGE_COMMUNICATION_FORM;
            }
        } else if (Constants.REGISTER_STRUCTURE_EVENT.equals(encounterType)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = Constants.JsonForm.THAILAND_ADD_STRUCTURE_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.NAMIBIA) {
                formName = Constants.JsonForm.NAMIBIA_ADD_STRUCTURE_FORM;
            } else {
                formName = Constants.JsonForm.ADD_STRUCTURE_FORM;
            }
        } else if (Constants.EventType.PAOT_EVENT.equals(encounterType) || Constants.Intervention.PAOT.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = Constants.JsonForm.THAILAND_PAOT_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = Constants.JsonForm.REFAPP_PAOT_FORM;
            } else {
                formName = Constants.JsonForm.PAOT_FORM;
            }
        } else if (Constants.Intervention.MDA_ADHERENCE.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
                formName = Constants.JsonForm.ZAMBIA_MDA_ADHERENCE_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = Constants.JsonForm.REFAPP_MDA_ADHERENCE_FORM;
            }
        } else if (Constants.Intervention.MDA_DISPENSE.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
                formName = Constants.JsonForm.ZAMBIA_MDA_DISPENSE_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = Constants.JsonForm.REFAPP_MDA_DISPENSE_FORM;
            }
        } else if (Constants.EventType.IRS_VERIFICATION.equals(encounterType) || Constants.Intervention.IRS_VERIFICATION.equals(taskCode)) {
            formName = Constants.JsonForm.ZAMBIA_IRS_VERIFICATION_FORM;
        }
        return formName;
    }

    @Override
    public boolean resetTaskInfo(@NonNull SQLiteDatabase db, @NonNull BaseTaskDetails taskDetails) {
        InteractorUtils interactorUtils = new InteractorUtils(RevealApplication.getInstance().getTaskRepository(), CoreLibrary.getInstance().context().getEventClientRepository(), RevealApplication.getInstance().getClientProcessor());
        return interactorUtils.archiveEventsForTask(db, taskDetails) && TaskUtils.getInstance().resetTask(taskDetails);
    }

    @Override
    public boolean archiveClient(String baseEntityId, boolean isFamily) {
        TaskRepository taskRepository = RevealApplication.getInstance().getTaskRepository();
        EventClientRepository eventClientRepository = CoreLibrary.getInstance().context().getEventClientRepository();
        ClientProcessorForJava clientProcessor = RevealApplication.getInstance().getClientProcessor();

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
            RevealApplication.getInstance().setSynced(false);
            Event archiveEvent = FamilyJsonFormUtils.createFamilyEvent(baseEntityId, Utils.getCurrentLocationId(),
                    null, isFamily ? FamilyConstants.EventType.ARCHIVE_FAMILY : FamilyConstants.EventType.ARCHIVE_FAMILY_MEMBER);
            archiveEvent.addObs(new Obs().withValue(now).withFieldCode("dateArchived").withFieldType("formsubmissionField"));

            JSONObject eventJson = new JSONObject(JsonFormUtils.gson.toJson(archiveEvent));
            eventJson.put(EventClientRepository.event_column.syncStatus.name(), BaseRepository.TYPE_Unsynced);
            eventClientRepository.addEvent(baseEntityId, eventJson);

            clientProcessor.processClient(Collections.singletonList(new EventClient(
                    JsonFormUtils.gson.fromJson(eventJson.toString(), org.smartregister.domain.Event.class),
                    JsonFormUtils.gson.fromJson(clientJsonObject.toString(), Client.class))), true);
            saved = true;

        } catch (Exception e) {
            Timber.e(e);
            saved = false;
        }
        return saved;
    }

    @Override
    public String getTranslatedIRSVerificationStatus(String status) {
        Context context = DrishtiApplication.getInstance().getApplicationContext();

        if (status == null)
            return context.getString(R.string.not_sprayed);

        switch (status) {
            case Constants.IRSVerificationStatus.SPRAYED:
                return context.getString(R.string.sprayed);
            case Constants.IRSVerificationStatus.NOT_SPRAYED:
                return context.getString(R.string.not_sprayed);
            case Constants.IRSVerificationStatus.NOT_FOUND_OR_VISITED:
                return context.getString(R.string.structure_not_found_or_visited_during_campaign);
            case Constants.IRSVerificationStatus.OTHER:
                return context.getString(R.string.other);
            default:
                return status;
        }
    }

    @Override
    public String getTranslatedBusinessStatus(String businessStatus) {
        Context context = RevealApplication.getInstance().getApplicationContext();

        if (businessStatus == null)
            return context.getString(R.string.not_eligible);
        switch (businessStatus) {
            case Constants.BusinessStatus.NOT_VISITED:
                return context.getString(R.string.not_visited);
            case Constants.BusinessStatus.NOT_SPRAYED:
                return context.getString(R.string.not_sprayed);
            case Constants.BusinessStatus.SPRAYED:
                return context.getString(R.string.sprayed);
            case Constants.BusinessStatus.NOT_SPRAYABLE:
                return context.getString(R.string.not_sprayable);
            case Constants.BusinessStatus.COMPLETE:
                return context.getString(R.string.complete);
            case Constants.BusinessStatus.INCOMPLETE:
                return context.getString(R.string.incomplete);
            case Constants.BusinessStatus.NOT_ELIGIBLE:
                return context.getString(R.string.not_eligible);
            case Constants.BusinessStatus.IN_PROGRESS:
                return context.getString(R.string.in_progress);
            case Constants.BusinessStatus.PARTIALLY_SPRAYED:
                return context.getString(R.string.partially_sprayed);
            default:
                return businessStatus;
        }
    }

    @Override
    public void formatCardDetails(CardDetails cardDetails) {
        if (cardDetails == null || cardDetails.getStatus() == null)
            return;
        // extract status color
        String status = cardDetails.getStatus();
        switch (status) {
            case Constants.BusinessStatus.NOT_SPRAYED:
            case Constants.BusinessStatus.INCOMPLETE:
            case Constants.BusinessStatus.IN_PROGRESS:
            case Constants.BusinessStatus.NONE_RECEIVED:
                cardDetails.setStatusColor(R.color.unsprayed);
                cardDetails.setStatusMessage(R.string.details_not_sprayed);
                break;
            case Constants.BusinessStatus.SPRAYED:
            case Constants.BusinessStatus.COMPLETE:
            case Constants.BusinessStatus.FULLY_RECEIVED:
                cardDetails.setStatusColor(R.color.sprayed);
                cardDetails.setStatusMessage(R.string.details_sprayed);
                cardDetails.setReason(null);
                break;
            case Constants.BusinessStatus.NOT_SPRAYABLE:
            case Constants.BusinessStatus.NOT_ELIGIBLE:
                cardDetails.setStatusColor(R.color.unsprayable);
                cardDetails.setStatusMessage(R.string.details_not_sprayable);
                cardDetails.setReason(null);
                break;
            case Constants.BusinessStatus.PARTIALLY_SPRAYED:
                cardDetails.setStatusColor(R.color.partially_sprayed);
                cardDetails.setStatusMessage(R.string.partially_sprayed);
                break;
            default:
                Timber.w("business status not defined :" + cardDetails.getStatus());
                break;
        }

    }

    @Override
    public void processServerConfigs() {
        RevealApplication.getInstance().processServerConfigs();
    }

    @Override
    public Map<String, Integer> populateLabels() {
        //Interventions
        Map<String, Integer> labelsMap = new HashMap<>();
        labelsMap.put(Constants.Intervention.IRS, R.string.irs);
        labelsMap.put(Constants.Intervention.MOSQUITO_COLLECTION, R.string.mosquito_collection);
        labelsMap.put(Constants.Intervention.LARVAL_DIPPING, R.string.larval_dipping);
        labelsMap.put(Constants.Intervention.BCC, R.string.bcc_code);
        labelsMap.put(Constants.Intervention.BEDNET_DISTRIBUTION, R.string.bednet_distribution);
        labelsMap.put(Constants.Intervention.BLOOD_SCREENING, R.string.blood_screening);
        labelsMap.put(Constants.Intervention.CASE_CONFIRMATION, R.string.case_confirmation);
        labelsMap.put(Constants.Intervention.REGISTER_FAMILY, R.string.register_family);
        labelsMap.put(Constants.Intervention.PAOT, R.string.paot);
        labelsMap.put(Constants.Intervention.MDA_DISPENSE, R.string.mda_dispense);
        labelsMap.put(Constants.Intervention.MDA_ADHERENCE, R.string.mda_adherence);
        labelsMap.put(Constants.Intervention.IRS_VERIFICATION, R.string.irs_verification);

        //Intervention Types
        labelsMap.put(Constants.InterventionType.OPERATIONAL_AREA, R.string.operational_area);
        labelsMap.put(Constants.InterventionType.STRUCTURE, R.string.structure);
        labelsMap.put(Constants.InterventionType.FAMILY, R.string.family);
        labelsMap.put(Constants.InterventionType.PERSON, R.string.person);

        //Business status
        labelsMap.put(Constants.BusinessStatus.NOT_VISITED, R.string.not_visited);
        labelsMap.put(Constants.BusinessStatus.NOT_SPRAYED, R.string.not_sprayed);
        labelsMap.put(Constants.BusinessStatus.NOT_SPRAYABLE, R.string.not_sprayable);
        labelsMap.put(Constants.BusinessStatus.SPRAYED, R.string.sprayed);

        labelsMap.put(Constants.BusinessStatus.COMPLETE, R.string.complete);
        labelsMap.put(Constants.BusinessStatus.INCOMPLETE, R.string.incomplete);
        labelsMap.put(Constants.BusinessStatus.NOT_ELIGIBLE, R.string.not_eligible);
        labelsMap.put(Constants.BusinessStatus.IN_PROGRESS, R.string.in_progress);
        
        return labelsMap;
    }

    @Override
    public void showBasicForm(BaseFormFragmentContract.View view, Context context, String formName) {
        RevealJsonFormUtils jsonFormUtils = new RevealJsonFormUtils();
        JSONObject formJSON = view.getJsonFormUtils().getFormJSON(context, formName, null, null);
        switch (formName) {

            case Constants.JsonForm.IRS_SA_DECISION_ZAMBIA:
            case Constants.JsonForm.CB_SPRAY_AREA_ZAMBIA:
            case Constants.JsonForm.MOBILIZATION_FORM_ZAMBIA:
                jsonFormUtils.populateServerOptions(RevealApplication.getInstance().getServerConfigs(),
                        formJSON, Constants.CONFIGURATION.SUPERVISORS, Constants.JsonForm.SUPERVISOR,
                        PreferencesUtil.getInstance().getCurrentDistrict());
                break;

            case Constants.JsonForm.IRS_FIELD_OFFICER_ZAMBIA:
                jsonFormUtils.populateServerOptions(RevealApplication.getInstance().getServerConfigs(),
                        formJSON, Constants.CONFIGURATION.FIELD_OFFICERS, Constants.JsonForm.FIELD_OFFICER,
                        PreferencesUtil.getInstance().getCurrentDistrict());
                break;

            case Constants.JsonForm.DAILY_SUMMARY_ZAMBIA:
                jsonFormUtils.populateServerOptions(RevealApplication.getInstance().getServerConfigs(),
                        formJSON, Constants.CONFIGURATION.TEAM_LEADERS, Constants.JsonForm.TEAM_LEADER,
                        PreferencesUtil.getInstance().getCurrentDistrict());

            case Constants.JsonForm.TEAM_LEADER_DOS_ZAMBIA:
                jsonFormUtils.populateServerOptions(RevealApplication.getInstance().getServerConfigs(),
                        formJSON, Constants.CONFIGURATION.SUPERVISORS, Constants.JsonForm.SUPERVISOR,
                        PreferencesUtil.getInstance().getCurrentDistrict());

                jsonFormUtils.populateServerOptions(RevealApplication.getInstance().getServerConfigs(),
                        formJSON, Constants.CONFIGURATION.DATA_COLLECTORS, Constants.JsonForm.DATA_COLLECTOR,
                        PreferencesUtil.getInstance().getCurrentDistrict());
                jsonFormUtils.populateServerOptions(RevealApplication.getInstance().getServerConfigs(),
                        formJSON, Constants.CONFIGURATION.DISTRICT_MANAGERS, Constants.JsonForm.DISTRICT_MANAGER,
                        PreferencesUtil.getInstance().getCurrentDistrict());

                break;

            case Constants.JsonForm.VERIFICATION_FORM_ZAMBIA:
                jsonFormUtils.populateServerOptions(RevealApplication.getInstance().getServerConfigs(),
                        formJSON, Constants.CONFIGURATION.FIELD_OFFICERS, Constants.JsonForm.FIELD_OFFICER,
                        PreferencesUtil.getInstance().getCurrentDistrict());

                jsonFormUtils.populateServerOptions(RevealApplication.getInstance().getServerConfigs(),
                        formJSON, Constants.CONFIGURATION.DATA_COLLECTORS, Constants.JsonForm.DATA_COLLECTOR,
                        PreferencesUtil.getInstance().getCurrentDistrict());

                break;
            default:
                break;
        }

        view.startForm(formJSON);
    }

    @Override
    public void onLocationValidated(@NonNull Context context, @NonNull BaseFormFragmentContract.View view, @NonNull BaseFormFragmentContract.Interactor interactor, @NonNull BaseTaskDetails baseTaskDetails, @NonNull Location structure) {
        PreferencesUtil prefsUtil = PreferencesUtil.getInstance();
        RevealJsonFormUtils jsonFormUtils = new RevealJsonFormUtils();
        if (!Constants.Intervention.REGISTER_FAMILY.equals(baseTaskDetails.getTaskCode())) {
            String formName = view.getJsonFormUtils().getFormName(null, baseTaskDetails.getTaskCode());
            if (StringUtils.isBlank(formName)) {
                view.displayError(R.string.opening_form_title, R.string.form_not_found);
            } else {
                JSONObject formJSON = view.getJsonFormUtils().getFormJSON(context, formName, baseTaskDetails, structure);
                if (Constants.Intervention.BEDNET_DISTRIBUTION.equals(baseTaskDetails.getTaskCode())) {
                    interactor.findNumberOfMembers(baseTaskDetails.getTaskEntity(), formJSON);
                    return;
                } else if (Constants.Intervention.CASE_CONFIRMATION.equals(baseTaskDetails.getTaskCode())) {
                    interactor.findMemberDetails(baseTaskDetails.getStructureId(), formJSON);
                    return;
                } else if (Constants.Intervention.IRS.equals(baseTaskDetails.getTaskCode()) && Country.NAMIBIA.equals(BuildConfig.BUILD_COUNTRY)) {
                    interactor.findSprayDetails(Constants.Intervention.IRS, structure.getId(), formJSON);
                } else if (Constants.Intervention.MDA_DISPENSE.equals(baseTaskDetails.getTaskCode()) || Constants.Intervention.MDA_ADHERENCE.equals(baseTaskDetails.getTaskCode())) {
                    jsonFormUtils.populateServerOptions(RevealApplication.getInstance().getServerConfigs(), formJSON, Constants.CONFIGURATION.MDA_CATCHMENT_AREAS, Constants.JsonForm.CATCHMENT_AREA, prefsUtil.getCurrentDistrict());
                    view.startForm(formJSON);
                } else {
                    view.startForm(formJSON);
                }
            }
        }

        view.hideProgressDialog();
    }

    @Override
    public String mainSelect(String mainCondition) {
        String tableName = Constants.DatabaseKeys.TASK_TABLE;
        SmartRegisterQueryBuilder queryBuilder = new SmartRegisterQueryBuilder();
        queryBuilder.selectInitiateMainTable(tableName, taskRegisterMainColumns(tableName), Constants.DatabaseKeys.ID);
        queryBuilder.customJoin(String.format(" JOIN %s ON %s.%s = %s.%s ",
                Constants.DatabaseKeys.STRUCTURES_TABLE, tableName, Constants.DatabaseKeys.FOR, Constants.DatabaseKeys.STRUCTURES_TABLE, Constants.DatabaseKeys.ID));
        queryBuilder.customJoin(String.format(" LEFT JOIN %s ON %s.%s = %s.%s ",
                Constants.DatabaseKeys.SPRAYED_STRUCTURES, tableName, Constants.DatabaseKeys.FOR, Constants.DatabaseKeys.SPRAYED_STRUCTURES, DBConstants.KEY.BASE_ENTITY_ID));
        queryBuilder.customJoin(String.format(" LEFT JOIN %s ON %s.%s = %s.%s ",
                FamilyConstants.TABLE_NAME.FAMILY, Constants.DatabaseKeys.STRUCTURES_TABLE, Constants.DatabaseKeys.ID, FamilyConstants.TABLE_NAME.FAMILY, Constants.DatabaseKeys.STRUCTURE_ID));
        return queryBuilder.mainCondition(mainCondition);
    }

    @Override
    public String nonRegisteredStructureTasksSelect(String mainCondition) {
        String tableName = Constants.Tables.TASK_TABLE;
        SmartRegisterQueryBuilder queryBuilder = new SmartRegisterQueryBuilder();
        queryBuilder.selectInitiateMainTable(tableName, taskRegisterMainColumns(tableName), Constants.DatabaseKeys.ID);
        queryBuilder.customJoin(String.format(" JOIN %s ON %s.%s = %s.%s ",
                Constants.DatabaseKeys.STRUCTURES_TABLE, tableName, Constants.DatabaseKeys.FOR, Constants.DatabaseKeys.STRUCTURES_TABLE, Constants.DatabaseKeys.ID));
        queryBuilder.customJoin(String.format(" LEFT JOIN %s ON %s.%s = %s.%s ",
                Constants.DatabaseKeys.SPRAYED_STRUCTURES, tableName, Constants.DatabaseKeys.FOR, Constants.DatabaseKeys.SPRAYED_STRUCTURES, DBConstants.KEY.BASE_ENTITY_ID));
        queryBuilder.customJoin(String.format(" LEFT JOIN %s ON %s.%s = %s.%s ",
                FamilyConstants.TABLE_NAME.FAMILY, Constants.DatabaseKeys.STRUCTURES_TABLE, Constants.DatabaseKeys.ID, FamilyConstants.TABLE_NAME.FAMILY, Constants.DatabaseKeys.STRUCTURE_ID));
        queryBuilder.mainCondition(mainCondition);
        queryBuilder.addCondition(String.format(" AND %s.%s IS NULL",
                FamilyConstants.TABLE_NAME.FAMILY, Constants.DatabaseKeys.STRUCTURE_ID));
        return queryBuilder.addCondition(String.format(" AND %s.%s != '%s'",
                tableName, Constants.DatabaseKeys.CODE, Constants.Intervention.BEDNET_DISTRIBUTION));
    }

    @Override
    public String groupedRegisteredStructureTasksSelect(String mainCondition) {
        String tableName = Constants.Tables.TASK_TABLE;
        SmartRegisterQueryBuilder structureTasksQueryBuilder = new SmartRegisterQueryBuilder();
        String[] columns = ArrayUtils.add(taskRegisterMainColumns(tableName), String.format("%s.%s||' '||%s.%s as %s ", "", "", "", Constants.DatabaseKeys.LAST_NAME, ""));//FAMILY_MEMBER, FIRST_NAME, FAMILY_MEMBER, LAST_NAME, FAMILY_MEMBER_NAMES));
        structureTasksQueryBuilder.selectInitiateMainTable(tableName, columns, Constants.DatabaseKeys.ID);
        structureTasksQueryBuilder.customJoin(String.format(" JOIN %s ON %s.%s = %s.%s ",
                Constants.DatabaseKeys.STRUCTURES_TABLE, tableName, Constants.DatabaseKeys.STRUCTURE_ID, Constants.DatabaseKeys.STRUCTURES_TABLE, Constants.DatabaseKeys.ID));
        structureTasksQueryBuilder.customJoin(String.format(" JOIN %s ON %s.%s = %s.%s  COLLATE NOCASE",
                FamilyConstants.TABLE_NAME.FAMILY, Constants.DatabaseKeys.STRUCTURES_TABLE, Constants.DatabaseKeys.ID, FamilyConstants.TABLE_NAME.FAMILY, Constants.DatabaseKeys.STRUCTURE_ID));
        structureTasksQueryBuilder.customJoin(String.format(" JOIN %s ON %s.%s = %s.%s  COLLATE NOCASE",
                FamilyConstants.TABLE_NAME.FAMILY_MEMBER, FamilyConstants.TABLE_NAME.FAMILY, Constants.DatabaseKeys.BASE_ENTITY_ID, FamilyConstants.TABLE_NAME.FAMILY_MEMBER, DBConstants.KEY.RELATIONAL_ID));
        structureTasksQueryBuilder.customJoin(String.format(" LEFT JOIN %s ON %s.%s = %s.%s ",
                Constants.DatabaseKeys.SPRAYED_STRUCTURES, tableName, Constants.DatabaseKeys.FOR, Constants.DatabaseKeys.SPRAYED_STRUCTURES, DBConstants.KEY.BASE_ENTITY_ID));
        structureTasksQueryBuilder.mainCondition(mainCondition);

        return String.format(" SELECT %s.* , SUM(CASE WHEN status='%s' THEN 1 ELSE 0 END ) AS %s , COUNT(_id ) AS %s, " +
                        "GROUP_CONCAT(%s || \"-\" || %s ) AS %s , GROUP_CONCAT(%s) as %s  FROM ( ",
                Constants.DatabaseKeys.GROUPED_TASKS, Task.TaskStatus.COMPLETED.toString(), Constants.DatabaseKeys.COMPLETED_TASK_COUNT, Constants.DatabaseKeys.TASK_COUNT, Constants.DatabaseKeys.CODE, Constants.DatabaseKeys.BUSINESS_STATUS, Constants.DatabaseKeys.GROUPED_STRUCTURE_TASK_CODE_AND_STATUS, "", "") + structureTasksQueryBuilder +//FAMILY_MEMBER_NAMES, FAMILY_MEMBER_NAMES) + structureTasksQueryBuilder +
                String.format(" ) AS %s GROUP BY %s ", Constants.DatabaseKeys.GROUPED_TASKS, Constants.DatabaseKeys.STRUCTURE_ID);
    }

    @Override
    public String[] taskRegisterMainColumns(String tableName) {
        return new String[]{
                tableName + "." + ID,
                tableName + "." + CODE,
                tableName + "." + FOR,
                tableName + "." + BUSINESS_STATUS,
                tableName + "." + STATUS,
                tableName + "." + REFERENCE_REASON,
                STRUCTURES_TABLE + "." + LATITUDE,
                STRUCTURES_TABLE + "." + LONGITUDE,
                STRUCTURES_TABLE + "." + NAME,
                SPRAYED_STRUCTURES + "." + STRUCTURE_NAME,
                SPRAYED_STRUCTURES + "." + FAMILY_NAME,
                SPRAYED_STRUCTURES + "." + SPRAY_STATUS,
                SPRAYED_STRUCTURES + "." + NOT_SRAYED_REASON,
                SPRAYED_STRUCTURES + "." + NOT_SRAYED_OTHER_REASON,
                STRUCTURES_TABLE + "." + ID + " AS " + STRUCTURE_ID,
                FAMILY + "." + FIRST_NAME,
                FAMILY + "." + HOUSE_NUMBER

        };
    }

    @Override
    public String familyRegisterTableName() {
        return metadata().familyRegister.tableName;
    }

    @Override
    public void saveCaseConfirmation(BaseContract.BaseInteractor baseInteractor, BaseContract.BasePresenter presenterCallBack, JSONObject jsonForm, String eventType) {
        AppExecutors appExecutors = RevealApplication.getInstance().getAppExecutors();
        TaskRepository taskRepository = RevealApplication.getInstance().getTaskRepository();
        RevealClientProcessor clientProcessor = (RevealClientProcessor) RevealApplication.getInstance().getClientProcessor();
        EventClientRepository eventClientRepository = CoreLibrary.getInstance().context().getEventClientRepository();

        PreferencesUtil prefsUtil = PreferencesUtil.getInstance();

        appExecutors.diskIO().execute(() -> {
            try {
                String baseEntityId = JsonFormUtils.getFieldValue(JsonFormUtils.fields(jsonForm), Constants.JsonForm.FAMILY_MEMBER);
                jsonForm.put(ENTITY_ID, baseEntityId);
                org.smartregister.domain.Event event = baseInteractor.saveEvent(jsonForm, eventType, CASE_CONFIRMATION);
                Client client = eventClientRepository.fetchClientByBaseEntityId(event.getBaseEntityId());
                String taskID = event.getDetails().get(Constants.Properties.TASK_IDENTIFIER);
                String businessStatus = clientProcessor.calculateBusinessStatus(event);
                Task task = taskRepository.getTaskByIdentifier(taskID);
                task.setForEntity(baseEntityId);
                task.setBusinessStatus(businessStatus);
                task.setStatus(Task.TaskStatus.COMPLETED);
                task.setSyncStatus(BaseRepository.TYPE_Created);
                taskRepository.addOrUpdate(task);
                Set<Task> removedTasks = new HashSet<>();
                for (Task bloodScreeningTask : taskRepository.getTasksByEntityAndCode(prefsUtil.getCurrentPlanId(),
                        Utils.getOperationalAreaLocation(prefsUtil.getCurrentOperationalArea()).getId(), baseEntityId, BLOOD_SCREENING)) {
                    bloodScreeningTask.setStatus(Task.TaskStatus.CANCELLED);
                    bloodScreeningTask.setSyncStatus(BaseRepository.TYPE_Created);
                    taskRepository.addOrUpdate(bloodScreeningTask);
                    removedTasks.add(bloodScreeningTask);
                }
                RevealApplication.getInstance().setSynced(false);
                clientProcessor.processClient(Collections.singletonList(new EventClient(event, client)), true);
                appExecutors.mainThread().execute(() -> {
                    ((StructureTasksContract.Presenter) presenterCallBack).onIndexConfirmationFormSaved(taskID, Task.TaskStatus.COMPLETED, businessStatus, removedTasks);
                });
            } catch (Exception e) {
                Timber.e("Error saving case confirmation data");
            }
        });
    }

    @Override
    public String calculateBusinessStatus(@NonNull org.smartregister.domain.Event event) {
        return ((RevealClientProcessor) RevealApplication.getInstance().getClientProcessor()).calculateBusinessStatus(event);
    }

    @Override
    public String getCurrentPlanId() {
        return PreferencesUtil.getInstance().getCurrentPlanId();
    }

    @Override
    public boolean getSynced() {
        return RevealApplication.getInstance().getSynced();
    }

    @Override
    public void setSynced(boolean synced) {
        RevealApplication.getInstance().setSynced(synced);
    }

    @Override
    public boolean isMyLocationComponentEnabled() {
        return RevealApplication.getInstance().isMyLocationComponentEnabled();
    }

    @Override
    public void setMyLocationComponentEnabled(boolean myLocationComponentEnabled) {
        RevealApplication.getInstance().setMyLocationComponentEnabled(myLocationComponentEnabled);
    }

    @Override
    public Task generateTaskFromStructureType(@NonNull Context context, @NonNull String structureId, @NonNull String structureType) {
        /*TaskUtils taskUtils = TaskUtils.getInstance();
        Task task = null;
        if (Constants.StructureType.RESIDENTIAL.equals(structureType) && Utils.isFocusInvestigationOrMDA()) {
            task = taskUtils.generateRegisterFamilyTask(applicationContext, structure.getId());
        } else {
            if (Constants.StructureType.RESIDENTIAL.equals(structureType)) {
                task = taskUtils.generateTask(applicationContext, structure.getId(), structure.getId(),
                        Constants.BusinessStatus.NOT_VISITED, Constants.Intervention.IRS, R.string.irs_task_description);
            } else if (Constants.StructureType.MOSQUITO_COLLECTION_POINT.equals(structureType)) {
                task = taskUtils.generateTask(applicationContext, structure.getId(), structure.getId(),
                        Constants.BusinessStatus.NOT_VISITED, Constants.Intervention.MOSQUITO_COLLECTION, R.string.mosquito_collection_task_description);
            } else if (Constants.StructureType.LARVAL_BREEDING_SITE.equals(structureType)) {
                task = taskUtils.generateTask(applicationContext, structure.getId(), structure.getId(),
                        Constants.BusinessStatus.NOT_VISITED, Constants.Intervention.LARVAL_DIPPING, R.string.larval_dipping_task_description);
            } else if (Constants.StructureType.POTENTIAL_AREA_OF_TRANSMISSION.equals(structureType)) {
                task = taskUtils.generateTask(applicationContext, structure.getId(), structure.getId(),
                        Constants.BusinessStatus.NOT_VISITED, PAOT, R.string.poat_task_description);
            }
        }*/
        return null;
    }

    @Override
    public void saveLocationInterventionForm(BaseContract.BaseInteractor baseInteractor, BaseContract.BasePresenter presenterCallBack, JSONObject jsonForm) {
        AppExecutors appExecutors = RevealApplication.getInstance().getAppExecutors();
        RevealClientProcessor clientProcessor = (RevealClientProcessor) RevealApplication.getInstance().getClientProcessor();
        String encounterType = null;
        String interventionType = null;
        try {
            encounterType = jsonForm.getString(Constants.JsonForm.ENCOUNTER_TYPE);
            if (encounterType.equals(Constants.SPRAY_EVENT)) {
                interventionType = IRS;
            } else if (encounterType.equals(Constants.MOSQUITO_COLLECTION_EVENT)) {
                interventionType = MOSQUITO_COLLECTION;
            } else if (encounterType.equals(Constants.LARVAL_DIPPING_EVENT)) {
                interventionType = LARVAL_DIPPING;
            } else if (encounterType.equals(Constants.BEDNET_DISTRIBUTION_EVENT)) {
                interventionType = BEDNET_DISTRIBUTION;
            } else if (encounterType.equals(Constants.BEHAVIOUR_CHANGE_COMMUNICATION)) {
                interventionType = BCC;
            } else if (encounterType.equals(Constants.EventType.PAOT_EVENT)) {
                interventionType = PAOT;
            } else if (encounterType.equals(Constants.EventType.MDA_DISPENSE)) {
                interventionType = Constants.Intervention.MDA_DISPENSE;
            } else if (encounterType.equals(Constants.EventType.MDA_ADHERENCE)) {
                interventionType = Constants.Intervention.MDA_ADHERENCE;
            } else if (encounterType.equals(Constants.EventType.IRS_VERIFICATION)) {
                interventionType = Constants.Intervention.IRS_VERIFICATION;
            }
        } catch (JSONException e) {
            Timber.e(e);
        }

        final String finalInterventionType = interventionType;
        final String finalEncounterType = encounterType;
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    org.smartregister.domain.Event event = baseInteractor.saveEvent(jsonForm, finalEncounterType, Constants.STRUCTURE);
                    clientProcessor.processClient(Collections.singletonList(new EventClient(event, null)), true);
                    appExecutors.mainThread().execute(new Runnable() {
                        @Override
                        public void run() {
                            String businessStatus = clientProcessor.calculateBusinessStatus(event);
                            String taskID = event.getDetails().get(Constants.Properties.TASK_IDENTIFIER);
                            presenterCallBack.onFormSaved(event.getBaseEntityId(), taskID, Task.TaskStatus.COMPLETED, businessStatus, finalInterventionType);
                        }
                    });
                } catch (JSONException e) {
                    Timber.e(e, "Error saving saving Form ");
                    presenterCallBack.onFormSaveFailure(finalEncounterType);
                }
            }
        };

        appExecutors.diskIO().execute(runnable);
    }

    @Override
    public void saveJsonForm(BaseContract.BaseInteractor baseInteractor, String json) {

        try {
            JSONObject jsonForm = new JSONObject(json);
            String encounterType = jsonForm.getString(ENCOUNTER_TYPE);
            boolean refreshMapOnEventSaved = true;
            switch (encounterType) {
                case REGISTER_STRUCTURE_EVENT:
                    baseInteractor.saveRegisterStructureForm(jsonForm);
                    break;
                case Constants.EventType.MDA_DISPENSE:
                case BLOOD_SCREENING_EVENT:
                case Constants.EventType.MDA_ADHERENCE:
                    baseInteractor.saveMemberForm(jsonForm, encounterType, BLOOD_SCREENING);
                    break;

                case CASE_CONFIRMATION_EVENT:
                    baseInteractor.saveCaseConfirmation(jsonForm, encounterType);
                    break;
                default:
                    baseInteractor.saveLocationInterventionForm(jsonForm);
                    if (!encounterType.equals(BEDNET_DISTRIBUTION_EVENT) && !encounterType.equals(Constants.EventType.IRS_VERIFICATION)) {
                        refreshMapOnEventSaved = false;
                    }
                    break;
            }
            RevealApplication.getInstance().setRefreshMapOnEventSaved(refreshMapOnEventSaved);
        } catch (Exception e) {
            Timber.e(e, "Error saving Json Form data");
        }
    }

    @Override
    public void openFilterActivity(Activity activity, TaskFilterParams filterParams) {
        Intent intent = new Intent(activity, FilterTasksActivity.class);
        intent.putExtra(Constants.Filter.FILTER_SORT_PARAMS, filterParams);
        activity.startActivityForResult(intent, Constants.RequestCode.REQUEST_CODE_FILTER_TASKS);
    }

    @Override
    public void openFamilyProfile(Activity activity, CommonPersonObjectClient family, BaseTaskDetails taskDetails) {
        Intent intent = new Intent(activity, org.smartregister.family.util.Utils.metadata().profileActivity);
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.FAMILY_BASE_ENTITY_ID, family.getCaseId());
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.FAMILY_HEAD, org.smartregister.family.util.Utils.getValue(family.getColumnmaps(), DBConstants.KEY.FAMILY_HEAD, false));
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.PRIMARY_CAREGIVER, org.smartregister.family.util.Utils.getValue(family.getColumnmaps(), DBConstants.KEY.PRIMARY_CAREGIVER, false));
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.FAMILY_NAME, org.smartregister.family.util.Utils.getValue(family.getColumnmaps(), DBConstants.KEY.FIRST_NAME, false));
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.GO_TO_DUE_PAGE, false);


        intent.putExtra(Constants.Properties.LOCATION_UUID, taskDetails.getStructureId());
        intent.putExtra(Constants.Properties.TASK_IDENTIFIER, taskDetails.getTaskId());
        intent.putExtra(Constants.Properties.TASK_BUSINESS_STATUS, taskDetails.getBusinessStatus());
        intent.putExtra(Constants.Properties.TASK_STATUS, taskDetails.getTaskStatus());

        activity.startActivity(intent);
    }

    @Override
    public void setTaskDetails(Activity activity, TaskRegisterAdapter taskAdapter, List<TaskDetails> tasks) {
        taskAdapter.setTaskDetails(tasks);
        if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
            new IndicatorsCalculatorTask(activity, tasks).execute();
        }
    }

    @Override
    public void showNotFoundPopup(Activity activity, String opensrpId) {
        if (activity != null) {
            NoMatchDialogFragment.launchDialog((BaseRegisterActivity) activity, "dialog", opensrpId);
        }

    }

    @Override
    public void startMapActivity(Activity activity, String searchViewText, TaskFilterParams taskFilterParams) {
        Intent intent = new Intent(activity, ListTasksActivity.class);
        if (taskFilterParams != null) {
            taskFilterParams.setSearchPhrase(searchViewText);
            intent.putExtra(FILTER_SORT_PARAMS, taskFilterParams);
        } else if (StringUtils.isNotBlank(searchViewText)) {
            intent.putExtra(FILTER_SORT_PARAMS, new TaskFilterParams(searchViewText));
        }

        activity.setResult(RESULT_OK, intent);
        activity.finish();
    }

    @Override
    public void onTaskRegisterBindViewHolder(@NonNull Context context, @NonNull TaskRegisterViewHolder viewHolder, @NonNull View.OnClickListener registerActionHandler, @NonNull TaskDetails task, int position) {
        Float distance = task.getDistanceFromUser();
        String name = task.getStructureName();
        String action = null;
        boolean hasIcon = false;
        if (Constants.Intervention.IRS.equals(task.getTaskCode())) {
            if (name == null) {
                name = task.getFamilyName() != null ? task.getFamilyName() : task.getStructureName() != null ? task.getStructureName() : context.getString(R.string.unenumerated_structure);
            }
            action = context.getString(R.string.record_status);
        } else if (Constants.Intervention.MOSQUITO_COLLECTION.equals(task.getTaskCode())) {
            name = context.getString(R.string.mosquito_collection_point);
            action = context.getString(R.string.record_mosquito_collection);
        } else if (Constants.Intervention.LARVAL_DIPPING.equals(task.getTaskCode())) {
            name = context.getString(R.string.larval_breeding_site);
            action = context.getString(R.string.record_larvacide);
        } else if (Constants.Intervention.BCC.equals(task.getTaskCode())) {
            viewHolder.setIcon(R.drawable.ic_bcc);
            name = context.getString(R.string.bcc);
            action = context.getString(R.string.record_bcc);
            hasIcon = true;
        } else if (Constants.Intervention.CASE_CONFIRMATION.equals(task.getTaskCode()) && task.getTaskCount() == null) {
            viewHolder.setIcon(R.drawable.ic_classification_details);
            viewHolder.setItemViewListener(task, registerActionHandler);
            name = context.getString(R.string.classification_details);
            action = context.getString(R.string.view);
            hasIcon = true;
        } else if (Constants.Intervention.PAOT.equals(task.getTaskCode())) {
            name = context.getString(R.string.card_view_paot);
            if (task.getBusinessStatus() != null) {
                action = CardDetailsUtil.getTranslatedBusinessStatus(task.getBusinessStatus()).replaceAll(" ", "\n");
            }
        } else {
            name = NOT_ELIGIBLE.equals(task.getBusinessStatus()) ? context.getString(R.string.ineligible_location) : task.getFamilyName();
            if (name == null) {
                name = task.getStructureName() != null ? task.getStructureName() : context.getString(R.string.unenumerated_structure);
            }
            if (task.getBusinessStatus() != null) {
                action = CardDetailsUtil.getTranslatedBusinessStatus(task.getBusinessStatus()).replaceAll(" ", "\n");
            }
        }
        viewHolder.setTaskName(name);
        CardDetails cardDetails = new CardDetails(task.getBusinessStatus());
        if (Task.TaskStatus.COMPLETED.name().equals(task.getTaskStatus())) {
            if (task.getBusinessStatus() != null) {
                action = CardDetailsUtil.getTranslatedBusinessStatus(task.getBusinessStatus()).replaceAll(" ", "\n");
            }
            CardDetailsUtil.formatCardDetails(cardDetails);
        }
        viewHolder.setTaskAction(action, task, cardDetails, registerActionHandler);
        viewHolder.setDistanceFromStructure(distance, task.isDistanceFromCenter());
        viewHolder.setTaskDetails(task.getBusinessStatus(), task.getTaskDetails());
        if (hasIcon) {
            viewHolder.hideDistanceFromStructure();
        } else {
            viewHolder.hideIcon();
        }

        if (StringUtils.isNotEmpty(task.getHouseNumber())) {
            viewHolder.showHouseNumber();
            viewHolder.setHouseNumber(context.getString(R.string.numero_sign) + " " + task.getHouseNumber());
        } else {
            viewHolder.hideHouseNumber();
        }
    }

    @NonNull
    @Override
    public org.smartregister.util.AppExecutors getAppExecutors() {
        return RevealApplication.getInstance().getAppExecutors();
    }
}
