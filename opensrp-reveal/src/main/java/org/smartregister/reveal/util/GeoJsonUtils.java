package org.smartregister.reveal.util;

import org.smartregister.domain.Location;
import org.smartregister.domain.Task;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.smartregister.reveal.interactor.ListTaskInteractor.gson;
import static org.smartregister.reveal.util.Constants.BusinessStatus.ADHERENCE_VISIT_DONE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BEDNET_DISTRIBUTED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BLOOD_SCREENING_COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FAMILY_REGISTERED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FULLY_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NONE_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.PARTIALLY_RECEIVED;
import static org.smartregister.reveal.util.Constants.GeoJSON.IS_INDEX_CASE;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.MDA_ADHERENCE;
import static org.smartregister.reveal.util.Constants.Intervention.MDA_DISPENSE;
import static org.smartregister.reveal.util.Constants.Intervention.REGISTER_FAMILY;
import static org.smartregister.reveal.util.Constants.Properties.FEATURE_SELECT_TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_TYPE;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_UUID;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_VERSION;
import static org.smartregister.reveal.util.Constants.Properties.STRUCTURE_NAME;
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
            boolean bloodScreeningDone = false;
            boolean familyRegTaskExists = false;
            boolean mdaAdhered = false;
            boolean fullyReceived;
            boolean nonReceived;
            boolean nonEligible;
            boolean partiallyReceived;
            int mdaDispenseTaskCount = 0 ;
            int fullyReceivedCount = 0;
            int nonReceivedCount = 0;
            int nonEligibleCount = 0;

            if (taskSet == null)
                continue;
            for (Task task : taskSet) {
                if (Utils.isResidentialStructure(task.getCode())) {

                    familyRegTaskExists = task.getCode().equals(REGISTER_FAMILY);
                    if (familyRegTaskExists && task.getBusinessStatus().equals(COMPLETE)) {
                        familyRegistered = true;
                    }

                    if (Utils.isFocusInvestigation()) {
                        if (task.getCode().equals(BEDNET_DISTRIBUTION) && task.getBusinessStatus().equals(COMPLETE)) {
                            bednetDistributed = true;
                        } else if (task.getCode().equals(BLOOD_SCREENING) && task.getBusinessStatus().equals(COMPLETE)) {
                            bloodScreeningDone = true;
                        }
                    } else if (Utils.isMDA()){
                        if (MDA_DISPENSE.equals(task.getCode())) {
                            mdaDispenseTaskCount++;
                            if ( FULLY_RECEIVED.equals(task.getBusinessStatus())) {
                                fullyReceivedCount++;
                            } else if ( NONE_RECEIVED.equals(task.getBusinessStatus())) {
                                nonReceivedCount++;
                            } else if( NOT_ELIGIBLE.equals(task.getBusinessStatus())) {
                                nonEligibleCount++;
                            }
                        } else if(MDA_ADHERENCE.equals(task.getCode()) && COMPLETE.equals(task.getBusinessStatus())) {
                            mdaAdhered = true;
                        }
                    }

                }
                taskProperties = new HashMap<>();
                taskProperties.put(TASK_IDENTIFIER, task.getIdentifier());
                if (Utils.isResidentialStructure(task.getCode()) && Utils.isFocusInvestigationOrMDA()) { // used to determine color of structure displayed on map
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
                taskProperties.put(STRUCTURE_NAME, structure.getProperties().getName());

            }

            // The assumption is that a register structure task always exists if the structure has
            // atleast one bednet distribution or blood screening task
            if ( Utils.isResidentialStructure(taskProperties.get(TASK_CODE)) ) {

                boolean familyRegTaskMissingOrFamilyRegComplete = familyRegistered || !familyRegTaskExists;

                if (Utils.isFocusInvestigation()) {

                    if (familyRegTaskMissingOrFamilyRegComplete &&
                            bednetDistributed && bloodScreeningDone) {
                        taskProperties.put(TASK_BUSINESS_STATUS, COMPLETE);
                    }  else if (familyRegTaskMissingOrFamilyRegComplete &&
                            !bednetDistributed && !bloodScreeningDone ) {
                        taskProperties.put(TASK_BUSINESS_STATUS, FAMILY_REGISTERED);
                    } else if (bednetDistributed && familyRegTaskMissingOrFamilyRegComplete) {
                        taskProperties.put(TASK_BUSINESS_STATUS, BEDNET_DISTRIBUTED);
                    } else if (bloodScreeningDone) {
                        taskProperties.put(TASK_BUSINESS_STATUS, BLOOD_SCREENING_COMPLETE);
                    } else {
                        taskProperties.put(TASK_BUSINESS_STATUS, NOT_VISITED);
                    }

                } else if (Utils.isMDA()) {

                    fullyReceived = (fullyReceivedCount == mdaDispenseTaskCount);
                    nonReceived = (nonReceivedCount == mdaDispenseTaskCount);
                    partiallyReceived = (!fullyReceived && nonReceivedCount > 0);
                    nonEligible = (nonEligibleCount == mdaDispenseTaskCount);

                    if (familyRegTaskMissingOrFamilyRegComplete && mdaAdhered) {
                        taskProperties.put(TASK_BUSINESS_STATUS, ADHERENCE_VISIT_DONE);
                    } else if (familyRegTaskMissingOrFamilyRegComplete && fullyReceived ) {
                        taskProperties.put(TASK_BUSINESS_STATUS, FULLY_RECEIVED);
                    } else if (familyRegTaskMissingOrFamilyRegComplete && partiallyReceived ) {
                        taskProperties.put(TASK_BUSINESS_STATUS, PARTIALLY_RECEIVED);
                    } else if (familyRegTaskMissingOrFamilyRegComplete && nonReceived ) {
                        taskProperties.put(TASK_BUSINESS_STATUS, NONE_RECEIVED);
                    } else if (familyRegTaskMissingOrFamilyRegComplete && nonEligible ) {
                        taskProperties.put(TASK_BUSINESS_STATUS, NOT_ELIGIBLE);
                    } else if (familyRegTaskMissingOrFamilyRegComplete &&
                            !mdaAdhered ) {
                        taskProperties.put(TASK_BUSINESS_STATUS, FAMILY_REGISTERED);
                    } else {
                        taskProperties.put(TASK_BUSINESS_STATUS, NOT_VISITED);
                    }

                }

            }

            structure.getProperties().setCustomProperties(taskProperties);

        }
        return gson.toJson(structures);
    }
}
