package org.smartregister.reveal.model;

import android.location.Location;

import androidx.annotation.NonNull;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.Utils;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import static org.smartregister.reveal.util.Constants.BusinessStatus.BEDNET_DISTRIBUTED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BLOOD_SCREENING_COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FAMILY_REGISTERED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FULLY_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.INELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NONE_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_DISPENSED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.PARTIALLY_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.SMC_COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.SPAQ_COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.TASKS_INCOMPLETE;
import static org.smartregister.reveal.util.Constants.COMMA;
import static org.smartregister.reveal.util.Constants.HYPHEN;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;
import static org.smartregister.reveal.util.Constants.Intervention.MDA_ADHERENCE;
import static org.smartregister.reveal.util.Constants.Intervention.MDA_DISPENSE;
import static org.smartregister.reveal.util.Constants.Intervention.MDA_DRUG_RECON;
import static org.smartregister.reveal.util.Constants.Intervention.REGISTER_FAMILY;
import static org.smartregister.reveal.util.Constants.MDA_ADHERENCE_COMPLETE_COUNT;
import static org.smartregister.reveal.util.Constants.MDA_DISPENSE_TASK_COUNT;
import static org.smartregister.reveal.util.Constants.MDA_DRUG_RECON_COMPLETE_COUNT;
import static org.smartregister.reveal.util.Constants.MDA_TASK_COUNT;
import static org.smartregister.reveal.util.Constants.TILDE;

/**
 * Created by samuelgithengi on 3/20/19.
 */
public class TaskDetails extends BaseTaskDetails implements Comparable<TaskDetails>, Serializable {

    private Location location;

    private String structureName;

    private String familyName;

    private float distanceFromUser;

    private String sprayStatus;

    private String taskDetails;

    private boolean distanceFromCenter;

    private Integer taskCount;

    private Integer completeTaskCount;

    private boolean familyRegistered;

    private boolean bednetDistributed;

    private boolean bloodScreeningDone;

    private String reasonReference;

    private String houseNumber;

    private boolean familyRegTaskExists;

    private boolean mdaAdhered;

    private boolean fullyReceived;

    private boolean partiallyReceived;

    private boolean noneReceived;

    private boolean notEligible;

    private String aggregateBusinessStatus;

    private int mdaTasksCount;

    private boolean eligibleNonCompleted;

    private String groupedTaskCodes;

    public TaskDetails(@NonNull String taskId) {
        super(taskId);
    }

    public Location getLocation() {
        return location;
    }

    public void setLocation(Location location) {
        this.location = location;
    }

    public String getStructureName() {
        return structureName;
    }

    public void setStructureName(String structureName) {
        this.structureName = structureName;
    }

    public String getFamilyName() {
        return familyName;
    }

    public void setFamilyName(String familyName) {
        this.familyName = familyName;
    }

    public float getDistanceFromUser() {
        return distanceFromUser;
    }

    public void setDistanceFromUser(float distanceFromUser) {
        this.distanceFromUser = distanceFromUser;
    }

    public String getSprayStatus() {
        return sprayStatus;
    }

    public void setSprayStatus(String sprayStatus) {
        this.sprayStatus = sprayStatus;
    }

    public String getTaskDetails() {
        return taskDetails;
    }

    public void setTaskDetails(String taskDetails) {
        this.taskDetails = taskDetails;
    }

    public boolean isDistanceFromCenter() {
        return distanceFromCenter;
    }

    public void setDistanceFromCenter(boolean distanceFromCenter) {
        this.distanceFromCenter = distanceFromCenter;
    }

    public void setTaskCount(Integer taskCount) {
        this.taskCount = taskCount;
    }

    public Integer getCompleteTaskCount() {
        return completeTaskCount;
    }

    public void setCompleteTaskCount(Integer completeTaskCount) {
        this.completeTaskCount = completeTaskCount;
    }

    public Integer getTaskCount() {
        return taskCount;
    }

    public boolean isFamilyRegistered() {
        return familyRegistered;
    }

    public void setFamilyRegistered(boolean familyRegistered) {
        this.familyRegistered = familyRegistered;
    }

    public boolean isBednetDistributed() {
        return bednetDistributed;
    }

    public void setBednetDistributed(boolean bednetDistributed) {
        this.bednetDistributed = bednetDistributed;
    }

    public boolean isBloodScreeningDone() {
        return bloodScreeningDone;
    }

    public void setBloodScreeningDone(boolean bloodScreeningDone) {
        this.bloodScreeningDone = bloodScreeningDone;
    }

    public boolean isFamilyRegTaskExists() {
        return familyRegTaskExists;
    }

    public void setFamilyRegTaskExists(boolean familyRegTaskExists) {
        this.familyRegTaskExists = familyRegTaskExists;
    }

    public boolean isMdaAdhered() {
        return mdaAdhered;
    }

    public void setMdaAdhered(boolean mdaAdhered) {
        this.mdaAdhered = mdaAdhered;
    }

    public boolean isFullyReceived() {
        return fullyReceived;
    }

    public void setFullyReceived(boolean fullyReceived) {
        this.fullyReceived = fullyReceived;
    }

    public boolean isPartiallyReceived() {
        return partiallyReceived;
    }

    public void setPartiallyReceived(boolean partiallyReceived) {
        this.partiallyReceived = partiallyReceived;
    }

    public boolean isNoneReceived() {
        return noneReceived;
    }

    public void setNoneReceived(boolean noneReceived) {
        this.noneReceived = noneReceived;
    }

    public boolean isNotEligible() {
        return notEligible;
    }

    public void setNotEligible(boolean notEligible) {
        this.notEligible = notEligible;
    }

    public boolean isEligibleNonCompleted() {
        return eligibleNonCompleted;
    }

    public void setEligibleNonCompleted(boolean eligibleNonCompleted) {
        this.eligibleNonCompleted = eligibleNonCompleted;
    }

    public void setGroupedTaskCodeStatus(String groupedTaskCodeStatusString) {
        setFamilyRegistered(false);
        setBednetDistributed(false);
        setBloodScreeningDone(false);
        setFamilyRegTaskExists(false);
        if (StringUtils.isEmpty(groupedTaskCodeStatusString)) {
            return;
        }
        String[] groupedTaskCodeStatusArray = groupedTaskCodeStatusString.split(COMMA);
        String MDA_DISPENSE_TASK_COUNT = "mda_dispense_task_count";

        Map<String, Integer> mdaStatusMap = new HashMap<>();
        mdaStatusMap.put(FULLY_RECEIVED, 0);
        mdaStatusMap.put(NONE_RECEIVED, 0);
        mdaStatusMap.put(NOT_ELIGIBLE, 0);
        mdaStatusMap.put(MDA_DISPENSE_TASK_COUNT, 0);
        mdaStatusMap.put(SMC_COMPLETE, 0);
        mdaStatusMap.put(NOT_DISPENSED, 0);
        mdaStatusMap.put(INELIGIBLE, 0);
        mdaStatusMap.put(MDA_TASK_COUNT, 0);
        mdaStatusMap.put(MDA_ADHERENCE, 0);
        mdaStatusMap.put(MDA_DRUG_RECON, 0);
        mdaStatusMap.put(MDA_ADHERENCE_COMPLETE_COUNT, 0);
        mdaStatusMap.put(MDA_DRUG_RECON_COMPLETE_COUNT, 0);

        boolean bloodScreeningExists = false;
        boolean caseConfirmed = false;
        for (int i = 0; i < groupedTaskCodeStatusArray.length; i++) {
            String[] taskCodeStatusArray = groupedTaskCodeStatusArray[i].split(HYPHEN);

            if (taskCodeStatusArray == null || taskCodeStatusArray.length != 2) {
                continue;
            }

            switch (taskCodeStatusArray[0]) {
                case REGISTER_FAMILY:
                    setFamilyRegTaskExists(true);
                    this.familyRegistered = COMPLETE.equals(taskCodeStatusArray[1]);
                    break;
                case BEDNET_DISTRIBUTION:
                    this.bednetDistributed = COMPLETE.equals(taskCodeStatusArray[1]);
                    break;
                case BLOOD_SCREENING:
                    if (!this.bloodScreeningDone) {
                        this.bloodScreeningDone = COMPLETE.equals(taskCodeStatusArray[1]);
                    }
                    bloodScreeningExists = true;
                    break;
                case CASE_CONFIRMATION:
                    caseConfirmed = COMPLETE.equals(taskCodeStatusArray[1]);
                    break;
                case MDA_ADHERENCE:
                    mdaStatusMap.put(MDA_TASK_COUNT, mdaStatusMap.get(MDA_TASK_COUNT) + 1);
                    mdaStatusMap.put(MDA_ADHERENCE, mdaStatusMap.get(MDA_ADHERENCE) + 1);
                    if (SPAQ_COMPLETE.equals(taskCodeStatusArray[1])) {
                        mdaStatusMap.put(MDA_ADHERENCE_COMPLETE_COUNT, mdaStatusMap.get(MDA_ADHERENCE_COMPLETE_COUNT) + 1);
                    }
                    this.mdaAdhered = TASKS_INCOMPLETE.equals(taskCodeStatusArray[1]) || COMPLETE.equals(taskCodeStatusArray[1]);
                    break;
                case MDA_DRUG_RECON:
                    mdaStatusMap.put(MDA_TASK_COUNT, mdaStatusMap.get(MDA_TASK_COUNT) + 1);
                    mdaStatusMap.put(MDA_DRUG_RECON, mdaStatusMap.get(MDA_DRUG_RECON) + 1);
                    if (COMPLETE.equals(taskCodeStatusArray[1])) {
                        mdaStatusMap.put(MDA_DRUG_RECON_COMPLETE_COUNT, mdaStatusMap.get(MDA_DRUG_RECON_COMPLETE_COUNT) + 1);
                    }
                    this.mdaAdhered = COMPLETE.equals(taskCodeStatusArray[1]);
                    break;
                case MDA_DISPENSE:
                    mdaStatusMap.put(MDA_DISPENSE_TASK_COUNT, mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT) + 1);
                    if (Utils.isCountryBuild(Country.NIGERIA)) {
                        handleSMCDispence(mdaStatusMap, taskCodeStatusArray);
                    } else {
                        handleMDADispence(mdaStatusMap, taskCodeStatusArray);
                    }
                default:
                    break;
            }
        }

        if (!bloodScreeningExists && caseConfirmed && !isBloodScreeningDone()) {
            setBloodScreeningDone(true);
        }

        if (!Utils.isCountryBuild(Country.NIGERIA)) {
            setFullyReceived(mdaStatusMap.get(FULLY_RECEIVED) == mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT));
            setNoneReceived(mdaStatusMap.get(NONE_RECEIVED) == mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT));
            setNotEligible(mdaStatusMap.get(NOT_ELIGIBLE) == mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT));
            setPartiallyReceived(!isFullyReceived() && (mdaStatusMap.get(FULLY_RECEIVED) > 0));
        } else {
            setMdaTasksCount(mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT));
            setCompositeBusinessStatus(mdaStatusMap);
        }

        setAggregateBusinessStatus(calculateAggregateBusinessStatus());
    }

    /**
     * @return the aggregate business status
     * @see org.smartregister.reveal.viewholder.TaskRegisterViewHolder getActionDrawable(TaskDetails task)
     * Calculates the aggregate/overall business status
     */
    private String calculateAggregateBusinessStatus() {
        if (Utils.isFocusInvestigation()) {
            if (isFamilyRegisteredOrNoTaskExists() && isBednetDistributed() && isBloodScreeningDone()) {
                return COMPLETE;
            } else if (isFamilyRegisteredOrNoTaskExists() && !isBednetDistributed() && !isBloodScreeningDone()) {
                return FAMILY_REGISTERED;
            } else if (isFamilyRegisteredOrNoTaskExists() && isBednetDistributed()) {
                return BEDNET_DISTRIBUTED;
            } else if (isBloodScreeningDone()) {
                return BLOOD_SCREENING_COMPLETE;
            }
        } else if (Utils.isMDA()) {
            if (isFamilyRegisteredOrNoTaskExists() && isMdaAdhered() && !Utils.isCountryBuild(Country.NIGERIA)) {
                return COMPLETE;
            } else if (isFamilyRegisteredOrNoTaskExists() && isFullyReceived()) {
                return Utils.isCountryBuild(Country.NIGERIA) ? SPAQ_COMPLETE : FULLY_RECEIVED;
            } else if (isFamilyRegisteredOrNoTaskExists() && isPartiallyReceived()) {
                return PARTIALLY_RECEIVED;
            } else if (isFamilyRegisteredOrNoTaskExists() && isNoneReceived()) {
                return Utils.isCountryBuild(Country.NIGERIA) ? NOT_DISPENSED : NONE_RECEIVED;
            } else if (isFamilyRegisteredOrNoTaskExists()) {
                return FAMILY_REGISTERED;
            }
        } else if (isNotEligible()) {
            return Utils.isCountryBuild(Country.NIGERIA) ? INELIGIBLE : NOT_ELIGIBLE;
        }
        return NOT_VISITED;
    }

    private boolean isFamilyRegisteredOrNoTaskExists() {
        return isFamilyRegistered() || !isFamilyRegTaskExists();
    }


    @Override
    public int compareTo(@NonNull TaskDetails other) {
        return Double.compare(distanceFromUser, other.getDistanceFromUser());
    }

    public void setReasonReference(String reasonReference) {
        this.reasonReference = reasonReference;
    }

    public String getReasonReference() {
        return reasonReference;
    }

    public String getHouseNumber() {
        return houseNumber;
    }

    public void setHouseNumber(String houseNumber) {
        this.houseNumber = houseNumber;
    }

    public String getAggregateBusinessStatus() {
        return aggregateBusinessStatus;
    }

    public void setAggregateBusinessStatus(String aggregateBusinessStatus) {
        this.aggregateBusinessStatus = aggregateBusinessStatus;
    }

    public String getGroupedTaskCodes() {
        return groupedTaskCodes;
    }

    public void setGroupedTaskCodes(String groupedTaskCodeStatusString) {
        StringBuilder stringBuilder = new StringBuilder();
        String[] groupedTaskCodeStatusArray = groupedTaskCodeStatusString.split(COMMA);
        for (int i = 0; i < groupedTaskCodeStatusArray.length; i++) {
            String[] taskCodeStatusArray = groupedTaskCodeStatusArray[i].split(HYPHEN);
            stringBuilder.append(TILDE).append(taskCodeStatusArray[0]);
        }
        this.groupedTaskCodes = stringBuilder.toString();
    }

    public void setMdaTasksCount(int mdaTasksCount) {
        this.mdaTasksCount = mdaTasksCount;
    }

    public int getMdaTasksCount() {
        return mdaTasksCount;
    }

    private void handleSMCDispence(Map<String, Integer> mdaStatusMap, String[] taskCodeStatusArray) {
        mdaStatusMap.put(MDA_TASK_COUNT, mdaStatusMap.get(MDA_TASK_COUNT) + 1);
        switch (taskCodeStatusArray[1]) {
            case SMC_COMPLETE:
                mdaStatusMap.put(SMC_COMPLETE, mdaStatusMap.get(SMC_COMPLETE) + 1);
                break;
            case NOT_DISPENSED:
                mdaStatusMap.put(NOT_DISPENSED, mdaStatusMap.get(NOT_DISPENSED) + 1);
                break;
            case INELIGIBLE:
                mdaStatusMap.put(INELIGIBLE, mdaStatusMap.get(INELIGIBLE) + 1);
                break;
            default:
                break;
        }
    }

    private void handleMDADispence(Map<String, Integer> mdaStatusMap, String[] taskCodeStatusArray) {
        switch (taskCodeStatusArray[1]) {
            case FULLY_RECEIVED:
                mdaStatusMap.put(FULLY_RECEIVED, mdaStatusMap.get(FULLY_RECEIVED) + 1);
                break;
            case NONE_RECEIVED:
                mdaStatusMap.put(NONE_RECEIVED, mdaStatusMap.get(NONE_RECEIVED) + 1);
                break;
            case NOT_ELIGIBLE:
                mdaStatusMap.put(NOT_ELIGIBLE, mdaStatusMap.get(NOT_ELIGIBLE) + 1);
                break;
            default:
                break;
        }
    }

    private void setCompositeBusinessStatus(Map<String, Integer> mdaStatusMap) {
        boolean singleStatusCompleteMdaTasks = (mdaStatusMap.get(SMC_COMPLETE)
                + mdaStatusMap.get(MDA_ADHERENCE_COMPLETE_COUNT)
                + mdaStatusMap.get(MDA_DRUG_RECON_COMPLETE_COUNT))
                == mdaStatusMap.get(MDA_TASK_COUNT) ;
        boolean multiStatusCompleteMdaTasks = (mdaStatusMap.get(SMC_COMPLETE)
                + mdaStatusMap.get(INELIGIBLE)
                + mdaStatusMap.get(MDA_ADHERENCE_COMPLETE_COUNT)
                + mdaStatusMap.get(MDA_DRUG_RECON_COMPLETE_COUNT))
                == mdaStatusMap.get(MDA_TASK_COUNT) ;
        // in complete tasks
        boolean hasCompletedDispence = mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT)
                == (mdaStatusMap.get(SMC_COMPLETE) + mdaStatusMap.get(NOT_DISPENSED) + mdaStatusMap.get(INELIGIBLE));

        boolean hasCompletedAdherence = mdaStatusMap.get(MDA_ADHERENCE_COMPLETE_COUNT) == mdaStatusMap.get(MDA_ADHERENCE);

        boolean hasCompletedDrugRecon = mdaStatusMap.get(MDA_DRUG_RECON_COMPLETE_COUNT) == mdaStatusMap.get(MDA_DRUG_RECON);

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
            setNoneReceived(true);
        } else if (singleStatusCompleteMdaTasks || multiStatusCompleteMdaTasks){
            setFullyReceived(true);
        } else if (mdaStatusMap.get(INELIGIBLE) == mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT)) {
            setNotEligible(true);
        } else if (hasNonCompletedTasks
                && ((mdaStatusMap.get(NOT_DISPENSED) > 0) || (mdaStatusMap.get(INELIGIBLE) > 0) || (mdaStatusMap.get(SMC_COMPLETE) > 0))
                && ((mdaStatusMap.get(NOT_DISPENSED) != mdaStatusMap.get(MDA_DISPENSE_TASK_COUNT)) )) { // not the only dispense task
            setPartiallyReceived(true);
        } else if (hasNoCompletedMDATask) {
            setEligibleNonCompleted(true);
        } else {
            setNoneReceived(true);
        }

    }

}
