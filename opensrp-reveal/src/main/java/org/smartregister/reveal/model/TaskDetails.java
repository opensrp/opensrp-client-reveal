package org.smartregister.reveal.model;

import com.mapbox.mapboxsdk.geometry.LatLng;

/**
 * Created by samuelgithengi on 3/20/19.
 */
public class TaskDetails {

    private String taskId;

    private String taskCode;

    private String taskEntity;

    private String businessStatus;

    private double latitude;

    private double longitude;

    private String structureName;

    private String familyName;

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

    public double getLatitude() {
        return latitude;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
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
}
