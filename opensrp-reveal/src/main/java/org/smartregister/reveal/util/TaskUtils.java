package org.smartregister.reveal.util;

import android.content.Context;

import org.joda.time.DateTime;
import org.smartregister.domain.Task;
import org.smartregister.repository.BaseRepository;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.Intervention;

import java.util.UUID;

/**
 * Created by samuelgithengi on 4/14/19.
 */
public class TaskUtils {

    public static void generateBloodScreeningTask(Context context, String entityId) {
        RevealApplication.getInstance().getAppExecutors().diskIO().execute(() -> {
            Task task = new Task();
            DateTime now = new DateTime();
            task.setIdentifier(UUID.randomUUID().toString());
            task.setCampaignIdentifier(PreferencesUtil.getInstance().getCurrentCampaignId());
            task.setGroupIdentifier(Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea()).getId());
            task.setStatus(Task.TaskStatus.READY);
            task.setBusinessStatus(BusinessStatus.NOT_VISITED);
            task.setPriority(3);
            task.setCode(Intervention.BLOOD_SCREENING);
            task.setDescription(context.getString(R.string.blood_screening_description));
            task.setFocus(Intervention.BLOOD_SCREENING);
            task.setForEntity(entityId);
            task.setExecutionStartDate(now);
            task.setAuthoredOn(now);
            task.setLastModified(now);
            task.setOwner(RevealApplication.getInstance().getContext().allSharedPreferences().fetchRegisteredANM());
            task.setSyncStatus(BaseRepository.TYPE_Created);
            RevealApplication.getInstance().getTaskRepository().addOrUpdate(task);
        });
    }

    public static Task generateBedNetDistributionTask(Context context, String entityId) {
        RevealApplication.getInstance().getAppExecutors().diskIO().execute(() -> {
            Task task = new Task();
            DateTime now = new DateTime();
            task.setIdentifier(UUID.randomUUID().toString());
            task.setCampaignIdentifier(PreferencesUtil.getInstance().getCurrentCampaignId());
            task.setGroupIdentifier(Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea()).getId());
            task.setStatus(Task.TaskStatus.READY);
            task.setBusinessStatus(BusinessStatus.NOT_VISITED);
            task.setPriority(3);
            task.setCode(Intervention.BEDNET_DISTRIBUTION);
            task.setDescription(context.getString(R.string.bednet_distribution_description));
            task.setFocus(Intervention.BEDNET_DISTRIBUTION);
            task.setForEntity(entityId);
            task.setExecutionStartDate(now);
            task.setAuthoredOn(now);
            task.setLastModified(now);
            task.setOwner(RevealApplication.getInstance().getContext().allSharedPreferences().fetchRegisteredANM());
            task.setSyncStatus(BaseRepository.TYPE_Created);
            RevealApplication.getInstance().getTaskRepository().addOrUpdate(task);
        });
    }
}
