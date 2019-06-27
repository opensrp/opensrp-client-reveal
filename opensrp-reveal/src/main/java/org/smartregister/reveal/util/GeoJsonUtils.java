package org.smartregister.reveal.util;

import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.reveal.BuildConfig;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.smartregister.reveal.interactor.ListTaskInteractor.gson;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BEDNET_DISTRIBUTED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BLOOD_SCREENING_COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FAMILY_REGISTERED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.GeoJSON.IS_INDEX_CASE;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.REGISTER_FAMILY;
import static org.smartregister.reveal.util.Constants.Properties.FEATURE_SELECT_TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_TYPE;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_UUID;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_VERSION;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.Properties.TASK_STATUS;

/**
 * Created by samuelgithengi on 1/7/19.
 */
public class GeoJsonUtils {


    public static String getGeoJsonFromStructuresAndTasks(List<Location> structures, Map<String, Set<Task>> tasks, String indexCase) {
        for (Location structure : structures) {
            Set<Task> taskSet = tasks.get(structure.getId());
            String groupedStructureTasksBusinessStatus = COMPLETE;
            HashMap<String, String> taskProperties = null;
            boolean familyRegistered = false;
            boolean bednetDistributed = false;
            boolean allBloodScreeningDone = true;
            int bloodScreeningCount = 0;
            if (taskSet == null)
                continue;
            for (Task task : taskSet) {
                if (Utils.isResidentialStructure(task.getCode())) {

                    if (task.getCode().equals(REGISTER_FAMILY) && task.getBusinessStatus().equals(COMPLETE)) {
                        familyRegistered = true;
                    } else if (task.getCode().equals(BEDNET_DISTRIBUTION) && task.getBusinessStatus().equals(COMPLETE)) {
                        bednetDistributed = true;
                    } else if (task.getCode().equals(BLOOD_SCREENING) ) {
                        if (!task.getBusinessStatus().equals(COMPLETE)) {
                            allBloodScreeningDone = false;
                        }
                        bloodScreeningCount++;
                    }

                }
                taskProperties = new HashMap<>();
                taskProperties.put(TASK_IDENTIFIER, task.getIdentifier());
                if (Utils.isResidentialStructure(task.getCode())) { // used to determine color of structure displayed on map
                    taskProperties.put(TASK_BUSINESS_STATUS, groupedStructureTasksBusinessStatus);
                } else {
                    taskProperties.put(TASK_BUSINESS_STATUS, task.getBusinessStatus());
                }
                taskProperties.put(FEATURE_SELECT_TASK_BUSINESS_STATUS, task.getBusinessStatus()); // used to determine action to take when a feature is selected
                taskProperties.put(TASK_STATUS, task.getStatus().name());
                taskProperties.put(TASK_CODE, task.getCode());

                if (indexCase != null && structure.getId().equals(indexCase)) {
                    taskProperties.put(IS_INDEX_CASE, Boolean.TRUE.toString());
                } else {
                    taskProperties.put(IS_INDEX_CASE, Boolean.FALSE.toString());
                }

                taskProperties.put(LOCATION_UUID, structure.getProperties().getUid());
                taskProperties.put(LOCATION_VERSION, structure.getProperties().getVersion() + "");
                taskProperties.put(LOCATION_TYPE, structure.getProperties().getType());

            }

            allBloodScreeningDone = (allBloodScreeningDone && bloodScreeningCount > 0);
            if (Utils.isResidentialStructure(taskProperties.get(TASK_CODE))) {
                if (familyRegistered && bednetDistributed && allBloodScreeningDone) {
                    taskProperties.put(TASK_BUSINESS_STATUS, COMPLETE);
                } else if (familyRegistered && !bednetDistributed && !allBloodScreeningDone) {
                    taskProperties.put(TASK_BUSINESS_STATUS, FAMILY_REGISTERED);
                } else if (bednetDistributed && familyRegistered) {
                    taskProperties.put(TASK_BUSINESS_STATUS, BEDNET_DISTRIBUTED);
                } else if (allBloodScreeningDone) {
                    taskProperties.put(TASK_BUSINESS_STATUS, BLOOD_SCREENING_COMPLETE);
                } else {
                    taskProperties.put(TASK_BUSINESS_STATUS, NOT_VISITED);
                }
            }

            structure.getProperties().setCustomProperties(taskProperties);

        }
        return gson.toJson(structures);
    }
}
