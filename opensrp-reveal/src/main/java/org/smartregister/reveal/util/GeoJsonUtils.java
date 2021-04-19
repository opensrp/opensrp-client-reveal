package org.smartregister.reveal.util;

import androidx.annotation.NonNull;

import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.model.StructureDetails;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.smartregister.reveal.interactor.ListTaskInteractor.gson;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BEDNET_DISTRIBUTED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BLOOD_SCREENING_COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FAMILY_NO_TASK_REGISTERED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FAMILY_REGISTERED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FULLY_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.INELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NONE_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_DISPENSED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.PARTIALLY_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.PARTIALLY_SPRAYED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.SMC_COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.SPAQ_COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.SPRAYED;
import static org.smartregister.reveal.util.Constants.GeoJSON.IS_INDEX_CASE;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;
import static org.smartregister.reveal.util.Constants.Intervention.MDA_ADHERENCE;
import static org.smartregister.reveal.util.Constants.Intervention.MDA_DISPENSE;
import static org.smartregister.reveal.util.Constants.Intervention.MDA_DRUG_RECON;
import static org.smartregister.reveal.util.Constants.Intervention.REGISTER_FAMILY;
import static org.smartregister.reveal.util.Constants.MDA_ADHERENCE_COMPLETE_COUNT;
import static org.smartregister.reveal.util.Constants.MDA_DRUG_RECON_COMPLETE_COUNT;
import static org.smartregister.reveal.util.Constants.MDA_TASK_COUNT;
import static org.smartregister.reveal.util.Constants.Properties.FAMILY_MEMBER_NAMES;
import static org.smartregister.reveal.util.Constants.Properties.FEATURE_SELECT_TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_TYPE;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_UUID;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_VERSION;
import static org.smartregister.reveal.util.Constants.Properties.STRUCTURE_NAME;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE_LIST;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.Properties.TASK_STATUS;

/**
 * Created by samuelgithengi on 1/7/19.
 */
public class GeoJsonUtils {

    private static final String MDA_DISPENSE_TASK_COUNT = "mda_dispense_task_count";

    public static String getGeoJsonFromStructuresAndTasks(List<Location> structures, Map<String, Set<Task>> tasks, String indexCase, Map<String, StructureDetails> structureNames) {
        for (Location structure : structures) {
            Set<Task> taskSet = tasks.get(structure.getId());
            HashMap<String, String> taskProperties = new HashMap<>();

            StringBuilder interventionList = new StringBuilder();

            Map<String, Integer> mdaStatusMap = new HashMap<>();
            mdaStatusMap.put(FULLY_RECEIVED, 0);
            mdaStatusMap.put(NONE_RECEIVED, 0);
            mdaStatusMap.put(NOT_ELIGIBLE, 0);
            mdaStatusMap.put(SMC_COMPLETE, 0);
            mdaStatusMap.put(NOT_DISPENSED, 0);
            mdaStatusMap.put(INELIGIBLE, 0);
            mdaStatusMap.put(NOT_VISITED, 0);
            mdaStatusMap.put(MDA_DISPENSE_TASK_COUNT, 0);
            mdaStatusMap.put(MDA_TASK_COUNT, 0);
            mdaStatusMap.put(MDA_ADHERENCE, 0);
            mdaStatusMap.put(MDA_DRUG_RECON, 0);
            mdaStatusMap.put(MDA_ADHERENCE_COMPLETE_COUNT, 0);
            mdaStatusMap.put(MDA_DRUG_RECON_COMPLETE_COUNT, 0);
            StateWrapper state = new StateWrapper();
            if (taskSet == null)
                continue;
            for (Task task : taskSet) {
                calculateState(task, state, mdaStatusMap);

                taskProperties = new HashMap<>();
                taskProperties.put(TASK_IDENTIFIER, task.getIdentifier());
                if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA && PARTIALLY_SPRAYED.equals(task.getBusinessStatus())) { // Set here for non residential structures
                    taskProperties.put(TASK_BUSINESS_STATUS, SPRAYED);
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
                interventionList.append(task.getCode());
                interventionList.append("~");

            }

            populateBusinessStatus(taskProperties, mdaStatusMap, state);

            taskProperties.put(TASK_CODE_LIST, interventionList.toString());
            if (structureNames.get(structure.getId()) != null) {
                taskProperties.put(STRUCTURE_NAME, structureNames.get(structure.getId()).getStructureName());
                taskProperties.put(FAMILY_MEMBER_NAMES, structureNames.get(structure.getId()).getFamilyMembersNames());
            }
            structure.getProperties().setCustomProperties(taskProperties);

        }
        return gson.toJson(structures);
    }

    private static void calculateState(Task task, StateWrapper state, @NonNull Map<String, Integer> mdaStatusMap) {
        if (Utils.isResidentialStructure(task.getCode())) {
            switch (task.getCode()) {
                case REGISTER_FAMILY:
                    state.familyRegTaskExists = true;
                    state.familyRegistered = COMPLETE.equals(task.getBusinessStatus());
                    state.ineligibleForFamReg = NOT_ELIGIBLE.equals(task.getBusinessStatus());
                    break;
                case BEDNET_DISTRIBUTION:
                    state.bednetDistributed = COMPLETE.equals(task.getBusinessStatus()) || NOT_ELIGIBLE.equals(task.getBusinessStatus());
                    break;
                case BLOOD_SCREENING:
                    if (!state.bloodScreeningDone) {
                        state.bloodScreeningDone = COMPLETE.equals(task.getBusinessStatus()) || NOT_ELIGIBLE.equals(task.getBusinessStatus());
                    }
                    state.bloodScreeningExists = true;
                    break;
                case CASE_CONFIRMATION:
                    state.caseConfirmed = COMPLETE.equals(task.getBusinessStatus());
                    break;
                case MDA_ADHERENCE:
                    state.mdaAdhered = COMPLETE.equals(task.getBusinessStatus()) || NOT_ELIGIBLE.equals(task.getBusinessStatus());
                    mdaStatusMap.put(MDA_TASK_COUNT, mdaStatusMap.get(MDA_TASK_COUNT) + 1);
                    mdaStatusMap.put(MDA_ADHERENCE, mdaStatusMap.get(MDA_ADHERENCE) + 1);
                    if (SPAQ_COMPLETE.equals(task.getBusinessStatus())) {
                        mdaStatusMap.put(MDA_ADHERENCE_COMPLETE_COUNT, mdaStatusMap.get(MDA_ADHERENCE_COMPLETE_COUNT) + 1);
                    }
                    break;
                case MDA_DRUG_RECON:
                    mdaStatusMap.put(MDA_TASK_COUNT, mdaStatusMap.get(MDA_TASK_COUNT) + 1);
                    mdaStatusMap.put(MDA_DRUG_RECON, mdaStatusMap.get(MDA_DRUG_RECON) + 1);
                    if (COMPLETE.equals(task.getBusinessStatus())) {
                        mdaStatusMap.put(MDA_DRUG_RECON_COMPLETE_COUNT, mdaStatusMap.get(MDA_DRUG_RECON_COMPLETE_COUNT) + 1);
                    }
                    break;
                case MDA_DISPENSE:
                    mdaStatusMap.put(MDA_TASK_COUNT, mdaStatusMap.get(MDA_TASK_COUNT) + 1);
                    populateMDAStatus(task, mdaStatusMap);
                    break;
                default:
                    break;
            }

        }
    }

    private static void populateMDAStatus(Task task, Map<String, Integer> mdaStatusMap) {
        mdaStatusMap.put(MDA_DISPENSE_TASK_COUNT, mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT) + 1);
        if (Utils.isCountryBuild(Country.NIGERIA)) {
            populateSMCDispenceStatus(task, mdaStatusMap);
            return;
        }
        switch (task.getBusinessStatus()) {
            case FULLY_RECEIVED:
                mdaStatusMap.put(FULLY_RECEIVED, mdaStatusMap.get(FULLY_RECEIVED) + 1);
                break;
            case NONE_RECEIVED:
                mdaStatusMap.put(NONE_RECEIVED, mdaStatusMap.get(NONE_RECEIVED) + 1);
                break;
            case NOT_ELIGIBLE:
                mdaStatusMap.put(NOT_ELIGIBLE, mdaStatusMap.get(NOT_ELIGIBLE) + 1);
                break;
        }
    }

    private static void populateBusinessStatus(HashMap<String, String> taskProperties, Map<String, Integer> mdaStatusMap, StateWrapper state) {
        // The assumption is that a register structure task always exists if the structure has
        // atleast one bednet distribution or blood screening task
        if (Utils.isResidentialStructure(taskProperties.get(TASK_CODE))) {

            boolean familyRegTaskMissingOrFamilyRegComplete = state.familyRegistered || !state.familyRegTaskExists;

            if (Utils.isFocusInvestigation()) {

                if (familyRegTaskMissingOrFamilyRegComplete &&
                        state.bednetDistributed && state.bloodScreeningDone) {
                    taskProperties.put(TASK_BUSINESS_STATUS, COMPLETE);
                } else if (familyRegTaskMissingOrFamilyRegComplete &&
                        !state.bednetDistributed && (!state.bloodScreeningDone || (!state.bloodScreeningExists && !state.caseConfirmed))) {
                    taskProperties.put(TASK_BUSINESS_STATUS, FAMILY_REGISTERED);
                } else if (state.bednetDistributed && familyRegTaskMissingOrFamilyRegComplete) {
                    taskProperties.put(TASK_BUSINESS_STATUS, BEDNET_DISTRIBUTED);
                } else if (state.bloodScreeningDone) {
                    taskProperties.put(TASK_BUSINESS_STATUS, BLOOD_SCREENING_COMPLETE);
                } else if (state.ineligibleForFamReg) {
                    taskProperties.put(TASK_BUSINESS_STATUS, NOT_ELIGIBLE);
                } else {
                    taskProperties.put(TASK_BUSINESS_STATUS, NOT_VISITED);
                }

            } else if (Utils.isMDA()) {


                if (!Utils.isCountryBuild(Country.NIGERIA)) {
                    state.fullyReceived = (mdaStatusMap.get(FULLY_RECEIVED).equals(mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT)));
                    state.nonReceived = (mdaStatusMap.get(NONE_RECEIVED).equals(mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT)));
                    state.nonEligible = (mdaStatusMap.get(NOT_ELIGIBLE).equals(mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT)));
                    state.partiallyReceived = (!state.fullyReceived && (mdaStatusMap.get(FULLY_RECEIVED) > 0));
                } else {
                    setCompositeBusinessStatus(mdaStatusMap, state);
                }

                if (familyRegTaskMissingOrFamilyRegComplete) {
                    if (mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT) == 0) {
                        taskProperties.put(TASK_BUSINESS_STATUS, FAMILY_NO_TASK_REGISTERED);
                    } else if (state.fullyReceived) {
                        taskProperties.put(TASK_BUSINESS_STATUS, COMPLETE);
                    } else if (state.partiallyReceived) {
                        taskProperties.put(TASK_BUSINESS_STATUS, PARTIALLY_RECEIVED);
                    } else if (state.nonReceived) {
                        taskProperties.put(TASK_BUSINESS_STATUS, NOT_DISPENSED);
                    } else if (state.nonEligible) {
                        taskProperties.put(TASK_BUSINESS_STATUS, INELIGIBLE);
                    } else {
                        taskProperties.put(TASK_BUSINESS_STATUS, FAMILY_REGISTERED);
                    }
                } else if (state.ineligibleForFamReg) {
                    taskProperties.put(TASK_BUSINESS_STATUS, NOT_ELIGIBLE);
                } else {
                    taskProperties.put(TASK_BUSINESS_STATUS, NOT_VISITED);
                }

            }

        }
    }

    private static class StateWrapper {
        private boolean familyRegistered = false;
        private boolean bednetDistributed = false;
        private boolean bloodScreeningDone = false;
        private boolean familyRegTaskExists = false;
        private boolean caseConfirmed = false;
        private boolean mdaAdhered = false;
        private boolean fullyReceived;
        private boolean nonReceived;
        private boolean nonEligible;
        private boolean partiallyReceived;
        private boolean bloodScreeningExists = false;
        private boolean ineligibleForFamReg = false;
        private boolean eligibleNonCompleted;
    }

    private static void populateSMCDispenceStatus(Task task, Map<String, Integer> mdaStatusMap) {
        switch (task.getBusinessStatus()) {
            case SMC_COMPLETE:
                mdaStatusMap.put(SMC_COMPLETE, mdaStatusMap.get(SMC_COMPLETE) + 1);
                break;
            case NOT_DISPENSED:
                mdaStatusMap.put(NOT_DISPENSED, mdaStatusMap.get(NOT_DISPENSED) + 1);
                break;
            case INELIGIBLE:
                mdaStatusMap.put(INELIGIBLE, mdaStatusMap.get(INELIGIBLE) + 1);
                break;
            case NOT_VISITED:
                mdaStatusMap.put(NOT_VISITED, mdaStatusMap.get(NOT_VISITED) + 1);
                break;
        }
    }

    private static void setCompositeBusinessStatus(Map<String, Integer> mdaStatusMap, StateWrapper state) {
        boolean singleStatusCompleteMdaTasks = (mdaStatusMap.get(SMC_COMPLETE)
                + mdaStatusMap.get(MDA_ADHERENCE_COMPLETE_COUNT)
                + mdaStatusMap.get(MDA_DRUG_RECON_COMPLETE_COUNT))
                == mdaStatusMap.get(MDA_TASK_COUNT);
        boolean multiStatusCompleteMdaTasks = (mdaStatusMap.get(SMC_COMPLETE)
                + mdaStatusMap.get(INELIGIBLE)
                + mdaStatusMap.get(MDA_ADHERENCE_COMPLETE_COUNT)
                + mdaStatusMap.get(MDA_DRUG_RECON_COMPLETE_COUNT))
                == mdaStatusMap.get(MDA_TASK_COUNT) ;
        // in complete tasks
        boolean hasCompletedDispence = mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT)
                == (mdaStatusMap.get(SMC_COMPLETE) + mdaStatusMap.get(NOT_DISPENSED) + mdaStatusMap.get(INELIGIBLE));

        boolean hasCompletedAdherence = (mdaStatusMap.get(MDA_ADHERENCE) > 0) && mdaStatusMap.get(MDA_ADHERENCE_COMPLETE_COUNT) == mdaStatusMap.get(MDA_ADHERENCE);

        boolean hasCompletedDrugRecon =  (mdaStatusMap.get(MDA_DRUG_RECON) > 0) &&  (mdaStatusMap.get(MDA_DRUG_RECON_COMPLETE_COUNT) == mdaStatusMap.get(MDA_DRUG_RECON));

        boolean hasNonCompletedTasks = !(hasCompletedDispence && hasCompletedAdherence && hasCompletedDrugRecon);

        // no complete task
        boolean hasNoCompletedMDATask = (mdaStatusMap.get(MDA_TASK_COUNT) > 0)
                && (mdaStatusMap.get(MDA_ADHERENCE_COMPLETE_COUNT) == 0)
                && (mdaStatusMap.get(MDA_DRUG_RECON_COMPLETE_COUNT) == 0)
                && ((mdaStatusMap.get(SMC_COMPLETE) + mdaStatusMap.get(NOT_DISPENSED) + mdaStatusMap.get(INELIGIBLE)) ==0);

        // multi status households
        boolean hasSMCComplete = mdaStatusMap.get(SMC_COMPLETE) > 0;
        boolean hasNotDispensed = mdaStatusMap.get(NOT_DISPENSED) > 0;
        boolean hasInEligible = mdaStatusMap.get(INELIGIBLE) > 0;
        boolean hasNotDispensedAndInEligibleOnly = hasNotDispensed && hasInEligible && (mdaStatusMap.get(INELIGIBLE) + mdaStatusMap.get(NOT_DISPENSED) == mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT));
        boolean hasNotDispensedAndSMCCompleteOnly = hasNotDispensed && hasSMCComplete && (mdaStatusMap.get(SMC_COMPLETE) + mdaStatusMap.get(NOT_DISPENSED) == mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT));
        boolean hasNotDispensedAndSMCCompleteAndInEligible = hasNotDispensed && hasSMCComplete && hasInEligible && (mdaStatusMap.get(SMC_COMPLETE) + mdaStatusMap.get(NOT_DISPENSED) + mdaStatusMap.get(INELIGIBLE)  == mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT));

        if (hasNotDispensedAndInEligibleOnly
                || hasNotDispensedAndSMCCompleteOnly
                || hasNotDispensedAndSMCCompleteAndInEligible) {
            state.nonReceived = true;
        } else if (mdaStatusMap.get(INELIGIBLE) == mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT)) {
            state.nonEligible = true;
        } else if (singleStatusCompleteMdaTasks || multiStatusCompleteMdaTasks){
            state.fullyReceived = true;
        } else if (hasNonCompletedTasks
                && ((mdaStatusMap.get(NOT_DISPENSED) > 0) || (mdaStatusMap.get(INELIGIBLE) > 0) || (mdaStatusMap.get(SMC_COMPLETE) > 0))
                && ((mdaStatusMap.get(NOT_DISPENSED) != mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT)) )) { // not the only dispense task
            state.partiallyReceived = true;
        } else if (hasNoCompletedMDATask) {
            state.eligibleNonCompleted = true;
        } else {
            state.nonReceived = true;
        }

    }
}
