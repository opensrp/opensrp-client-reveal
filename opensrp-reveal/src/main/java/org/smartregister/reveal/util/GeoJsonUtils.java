package org.smartregister.reveal.util;

import org.smartregister.domain.Location;
import org.smartregister.domain.Task;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.smartregister.reveal.interactor.ListTaskInteractor.gson;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.GeoJSON.IS_INDEX_CASE;
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
            if (taskSet == null)
                continue;
            for (Task task : taskSet) {
                if (Utils.isResidentialStructure(task.getCode()) && !COMPLETE.equals(task.getBusinessStatus())) {
                    groupedStructureTasksBusinessStatus = NOT_VISITED; // so as to display the residential structure in yellow
                }
                HashMap<String, String> taskProperties = new HashMap<>();
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
                structure.getProperties().setCustomProperties(taskProperties);
            }
        }
        return gson.toJson(structures);
    }
}
