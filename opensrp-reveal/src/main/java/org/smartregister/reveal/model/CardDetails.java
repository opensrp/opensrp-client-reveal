package org.smartregister.reveal.model;

/**
 * @author Vincent Karuri
 */
public class CardDetails {

    private String sprayStatus;
    private String propertyType;
    private String sprayDate;
    private String sprayOperator;
    private String familyHead;
    private int statusMessage;
    private int statusColor;
    private String reason;

    public CardDetails(String sprayStatus, String propertyType, String sprayDate, String sprayOperator, String familyHead, String reason) {
        this.sprayStatus = sprayStatus;
        this.propertyType = propertyType;
        this.sprayDate = sprayDate;
        this.sprayOperator = sprayOperator;
        this.familyHead = familyHead;
        this.reason = reason;
    }

    public String getSprayStatus() {
        return sprayStatus;
    }

    public void setSprayStatus(String sprayStatus) {
        this.sprayStatus = sprayStatus;
    }

    public String getPropertyType() {
        return propertyType;
    }

    public void setPropertyType(String propertyType) {
        this.propertyType = propertyType;
    }

    public String getSprayDate() {
        return sprayDate;
    }

    public void setSprayDate(String date) {
        this.sprayDate = date;
    }

    public String getSprayOperator() {
        return sprayOperator;
    }

    public void setSprayOperator(String sprayOperator) {
        this.sprayOperator = sprayOperator;
    }

    public String getFamilyHead() {
        return familyHead;
    }

    public void setFamilyHead(String familyHead) {
        this.familyHead = familyHead;
    }

    public int getStatusColor() {
        return statusColor;
    }

    public void setStatusColor(int statusColor) {
        this.statusColor = statusColor;
    }

    public int getStatusMessage() {
        return statusMessage;
    }

    public void setStatusMessage(int statusMessage) {
        this.statusMessage = statusMessage;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }
}
