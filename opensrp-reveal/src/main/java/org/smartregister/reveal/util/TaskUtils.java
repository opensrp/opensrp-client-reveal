package org.smartregister.reveal.util;

import android.content.Context;
import android.support.annotation.StringRes;

import org.joda.time.DateTime;
import org.smartregister.domain.Action;
import org.smartregister.domain.PlanDefinition;
import org.smartregister.domain.Task;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.PlanDefinitionRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.Intervention;

import java.util.UUID;

/**
 * Created by samuelgithengi on 4/14/19.
 */
public class TaskUtils {

    private TaskRepository taskRepository;

    private PlanDefinitionRepository planRepository;

    private AllSharedPreferences sharedPreferences;

    private PreferencesUtil prefsUtil;

    private static TaskUtils instance;

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
        task.setStatus(Task.TaskStatus.READY);
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
        return task;
    }


    public Task generateRegisterFamilyTask(Context context, String entityId) {
        return generateTask(context, entityId, entityId, BusinessStatus.NOT_VISITED, Intervention.REGISTER_FAMILY,
                R.string.register_family_description);

    }

    public void generateMDADispenseTask(Context context, String entityId, String structureId) {
        generateTask(context, entityId, structureId, BusinessStatus.NOT_VISITED, Intervention.MDA_DISPENSE,
                R.string.mda_dispense_desciption);
    }

    public void generateMDAAdherenceTask(Context context, String entityId, String structureId) {
        generateTask(context, entityId, structureId, BusinessStatus.NOT_VISITED, Intervention.MDA_ADHERENCE,
                R.string.mda_adherence_desciption);
    }
}
