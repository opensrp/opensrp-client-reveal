package org.smartregister.reveal.model;

import android.location.Location;
import android.support.annotation.NonNull;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.reveal.util.Constants;

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

    private boolean allBloodScreeningDone;

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

    public boolean isAllBloodScreeningDone() {
        return allBloodScreeningDone;
    }

    public void setAllBloodScreeningDone(boolean allBloodScreeningDone) {
        this.allBloodScreeningDone = allBloodScreeningDone;
    }

    public void setGroupedTaskCodeStatus(String groupedTaskCodeStatusString) {
        setFamilyRegistered(false);
        setBednetDistributed(false);
        setAllBloodScreeningDone(true);
        if (StringUtils.isEmpty(groupedTaskCodeStatusString)) {
            return;
        }
        String[] groupedTaskCodeStatusArray = groupedTaskCodeStatusString.split(",");
        for (int i = 0; i < groupedTaskCodeStatusArray.length; i++) {
            String[] taskCodeStatusArray = groupedTaskCodeStatusArray[i].split("-");
            if (taskCodeStatusArray[0].equals(REGISTER_FAMILY) && taskCodeStatusArray[1].equals(Constants.BusinessStatus.COMPLETE)) {
                setFamilyRegistered(true);
            } else if (taskCodeStatusArray[0].equals(BEDNET_DISTRIBUTION) && taskCodeStatusArray[1].equals(Constants.BusinessStatus.COMPLETE)) {
                setBednetDistributed(true);
            } else if (taskCodeStatusArray[0].equals(BLOOD_SCREENING) && !taskCodeStatusArray[1].equals(Constants.BusinessStatus.COMPLETE)) {
                setAllBloodScreeningDone(false);
            }
        }
    }

    @Override
    public int compareTo(@NonNull TaskDetails other) {
        return Double.compare(distanceFromUser, other.getDistanceFromUser());
    }
}
