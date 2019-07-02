package org.smartregister.reveal.model;

import android.location.Location;
import android.support.annotation.NonNull;

import org.apache.commons.lang3.StringUtils;

import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.COMMA;
import static org.smartregister.reveal.util.Constants.HYPHEN;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.REGISTER_FAMILY;

/**
 * Created by samuelgithengi on 3/20/19.
 */
public class TaskDetails extends BaseTaskDetails implements Comparable<TaskDetails> {

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

    public void setGroupedTaskCodeStatus(String groupedTaskCodeStatusString) {
        setFamilyRegistered(false);
        setBednetDistributed(false);
        setBloodScreeningDone(false);
        if (StringUtils.isEmpty(groupedTaskCodeStatusString)) {
            return;
        }
        String[] groupedTaskCodeStatusArray = groupedTaskCodeStatusString.split(COMMA);
        for (int i = 0; i < groupedTaskCodeStatusArray.length; i++) {
            String[] taskCodeStatusArray = groupedTaskCodeStatusArray[i].split(HYPHEN);

            if (taskCodeStatusArray == null || taskCodeStatusArray.length != 2) {
                continue;
            }
            if (REGISTER_FAMILY.equals(taskCodeStatusArray[0]) && COMPLETE.equals(taskCodeStatusArray[1])) {
                setFamilyRegistered(true);
            } else if (BEDNET_DISTRIBUTION.equals(taskCodeStatusArray[0]) && COMPLETE.equals(taskCodeStatusArray[1])) {
                setBednetDistributed(true);
            }  else if (BLOOD_SCREENING.equals(taskCodeStatusArray[0]) && COMPLETE.equals(taskCodeStatusArray[1])) {
                setBloodScreeningDone(true);
            }
        }
    }

    @Override
    public int compareTo(@NonNull TaskDetails other) {
        return Double.compare(distanceFromUser, other.getDistanceFromUser());
    }
}
