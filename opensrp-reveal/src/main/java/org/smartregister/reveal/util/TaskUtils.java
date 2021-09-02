package org.smartregister.reveal.util;

import android.content.Context;
import androidx.annotation.StringRes;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.joda.time.DateTime;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.domain.Action;
import org.smartregister.domain.PlanDefinition;
import org.smartregister.domain.Task;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.PlanDefinitionRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.model.BaseTaskDetails;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.Intervention;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import timber.log.Timber;

import static org.smartregister.domain.Task.TaskStatus.READY;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CODE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FOR;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_TABLE;

/**
 * Created by samuelgithengi on 4/14/19.
 */
public class TaskUtils {

    private TaskRepository taskRepository;

    private PlanDefinitionRepository planRepository;

    private AllSharedPreferences sharedPreferences;

    private PreferencesUtil prefsUtil;

    private static TaskUtils instance;

    private RevealApplication revealApplication;

    public static TaskUtils getInstance() {
        if (instance == null) {
            instance = new TaskUtils();
        }
        return instance;
    }

    private TaskUtils() {
        taskRepository = RevealApplication.getInstance().getTaskRepository();
        sharedPreferences = RevealApplication.getInstance().getContext().allSharedPreferences();
        prefsUtil = PreferencesUtil.getInstance();
        planRepository = RevealApplication.getInstance().getPlanDefinitionRepository();
        revealApplication = RevealApplication.getInstance();
    }

    public void generateBloodScreeningTask(Context context, String entityId, String structureId) {
        generateTask(context, entityId, structureId, BusinessStatus.NOT_VISITED, Intervention.BLOOD_SCREENING,
                R.string.blood_screening_description);

    }

    public void generateBedNetDistributionTask(Context context, String entityId) {
        generateTask(context, entityId, entityId, BusinessStatus.NOT_VISITED, Intervention.BEDNET_DISTRIBUTION,
                R.string.bednet_distribution_description);
    }

    public Task generateTask(Context context, String entityId, String structureId, String businessStatus, String intervention, @StringRes int description) {
        Task task = new Task();
        DateTime now = new DateTime();
        task.setIdentifier(UUID.randomUUID().toString());
        task.setPlanIdentifier(prefsUtil.getCurrentPlanId());
        task.setGroupIdentifier(Utils.getOperationalAreaLocation(prefsUtil.getCurrentOperationalArea()).getId());
        task.setStatus(READY);
        task.setBusinessStatus(businessStatus);
        task.setPriority(3);
        task.setCode(intervention);
        task.setDescription(context.getString(description));
        PlanDefinition currentPlan = planRepository.findPlanDefinitionById(prefsUtil.getCurrentPlanId());
        if (currentPlan != null && currentPlan.getActions() != null) {
            for (Action action : currentPlan.getActions()) {
                if (intervention.equals(action.getCode())) {
                    task.setFocus(action.getIdentifier());
                    continue;
                }
            }
        }
        task.setForEntity(entityId);
        task.setStructureId(structureId);
        task.setExecutionStartDate(now);
        task.setAuthoredOn(now);
        task.setLastModified(now);
        task.setOwner(sharedPreferences.fetchRegisteredANM());
        task.setSyncStatus(BaseRepository.TYPE_Created);
        taskRepository.addOrUpdate(task);
        revealApplication.setSynced(false);
        return task;
    }


    public Task generateRegisterFamilyTask(Context context, String entityId) {
        return generateTask(context, entityId, entityId, BusinessStatus.NOT_VISITED, Intervention.REGISTER_FAMILY,
                R.string.register_family_description);

    }

    // Child SMC Task

    public void generateMDADispenseTask(Context context, String entityId, String structureId) {
        generateTask(context, entityId, structureId, BusinessStatus.NOT_VISITED, Intervention.MDA_DISPENSE,
                R.string.mda_dispense_desciption);
    }

    // SPAQ Task

    public void generateMDAAdherenceTask(Context context, String entityId, String structureId, String admininistedSpaq) {
        // HEADS UP
        if ("Yes".equalsIgnoreCase(admininistedSpaq)) {
            Set<Task> tasks = taskRepository.getTasksByEntityAndCode(prefsUtil.getCurrentPlanId(),
                    Utils.getOperationalAreaLocation(prefsUtil.getCurrentOperationalArea()).getId(), entityId, Intervention.MDA_ADHERENCE);

            if (tasks == null || tasks.isEmpty()) {
                generateTask(context, entityId, structureId, BusinessStatus.NOT_VISITED, Intervention.MDA_ADHERENCE,
                        R.string.mda_adherence_desciption);
            }
        }
    }

    // Drug Recon Task
    public void generateMDAStructureDrug(Context context, String entityId, String structureId) {
        Set<Task> tasks = taskRepository.getTasksByEntityAndCode(prefsUtil.getCurrentPlanId(),
                Utils.getOperationalAreaLocation(prefsUtil.getCurrentOperationalArea()).getId(), entityId, Intervention.MDA_DRUG_RECON);
        if (tasks == null || tasks.isEmpty()) {
            generateTask(context, entityId, structureId, BusinessStatus.NOT_VISITED, Intervention.MDA_DRUG_RECON,
                    R.string.mda_adherence_desciption);
        }
    }

    public void tagEventTaskDetails(List<Event> events, SQLiteDatabase sqLiteDatabase) {
        for (Event event : events) {
            Cursor cursor = null;
            try {
                cursor = sqLiteDatabase.rawQuery(String.format("select * from %s where %s =? and %s =? and %s =? limit 1", TASK_TABLE, FOR, STATUS, CODE),
                        new String[]{event.getBaseEntityId(), Task.TaskStatus.COMPLETED.name(), Intervention.IRS});
                while (cursor.moveToNext()) {
                    Task task = taskRepository.readCursor(cursor);
                    event.addDetails(Constants.Properties.TASK_IDENTIFIER, task.getIdentifier());
                    event.addDetails(Constants.Properties.TASK_BUSINESS_STATUS, task.getBusinessStatus());
                    event.addDetails(Constants.Properties.TASK_STATUS, task.getStatus().name());
                    event.addDetails(Constants.Properties.LOCATION_ID, task.getForEntity());
                    event.addDetails(Constants.Properties.APP_VERSION_NAME, BuildConfig.VERSION_NAME);
                    event.setLocationId(task.getGroupIdentifier());
                }

            } finally {
                if (cursor != null) {
                    cursor.close();
                }
            }

        }
    }

    public boolean resetTask(BaseTaskDetails taskDetails) {

        boolean taskResetSuccessful = false;
        try {
            Task task = taskRepository.getTaskByIdentifier(taskDetails.getTaskId());
            String operationalAreaId = Utils.getOperationalAreaLocation(prefsUtil.getCurrentOperationalArea()).getId();

            if (Intervention.CASE_CONFIRMATION.equals(taskDetails.getTaskCode())) {
                task.setForEntity(operationalAreaId);
            }

            task.setBusinessStatus(BusinessStatus.NOT_VISITED);
            task.setStatus(READY);
            task.setLastModified(new DateTime());
            task.setSyncStatus(BaseRepository.TYPE_Unsynced);
            taskRepository.addOrUpdate(task);
            revealApplication.setSynced(false);

            revealApplication.setRefreshMapOnEventSaved(true);

            taskResetSuccessful = true;
        } catch (Exception e) {
            Timber.e(e);
        }

        return taskResetSuccessful;

    }


}
