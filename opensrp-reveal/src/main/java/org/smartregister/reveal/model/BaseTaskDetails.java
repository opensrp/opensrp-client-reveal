package org.smartregister.reveal.model;

import androidx.annotation.NonNull;

/**
 * Created by samuelgithengi on 4/11/19.
 */
public class BaseTaskDetails {

    private String taskId;

    private String taskCode;

    private String taskEntity;

    private String businessStatus;

    private String taskStatus;

    private String structureId;

    private  String familyMemberNames;

    public BaseTaskDetails(@NonNull String taskId) {
        this.taskId = taskId;
    }

    public String getTaskId() {
        return taskId;
    }

    public void setTaskId(String taskId) {
        this.taskId = taskId;
    }

    public String getTaskCode() {
        return taskCode;
    }

    public void setTaskCode(String taskCode) {
        this.taskCode = taskCode;
    }

    public String getTaskEntity() {
        return taskEntity;
    }

    public void setTaskEntity(String taskEntity) {
        this.taskEntity = taskEntity;
    }

    public String getBusinessStatus() {
        return businessStatus;
    }

    public void setBusinessStatus(String businessStatus) {
        this.businessStatus = businessStatus;
    }

    public String getTaskStatus() {
        return taskStatus;
    }

    public void setTaskStatus(String taskStatus) {
        this.taskStatus = taskStatus;
    }

    public String getStructureId() {
        return structureId;
    }

    public void setStructureId(String structureId) {
        this.structureId = structureId;
    }

    public String getFamilyMemberNames() {
        return familyMemberNames;
    }

    public void setFamilyMemberNames(String familyMemberNames) {
        this.familyMemberNames = familyMemberNames;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof BaseTaskDetails))
            return false;
        BaseTaskDetails other = (BaseTaskDetails) obj;
        return getTaskId().equals(other.getTaskId());
    }
}
