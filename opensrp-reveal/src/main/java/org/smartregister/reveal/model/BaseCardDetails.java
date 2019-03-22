package org.smartregister.reveal.model;

/**
 * Created by samuelgithengi on 3/22/19.
 */
public class BaseCardDetails {

    private String sprayStatus;
    private int statusMessage;
    private Integer statusColor;
    private String reason;

    public BaseCardDetails(String sprayStatus) {
        this.sprayStatus = sprayStatus;
    }

    public String getSprayStatus() {
        return sprayStatus;
    }

    public void setSprayStatus(String sprayStatus) {
        this.sprayStatus = sprayStatus;
    }

    public int getStatusMessage() {
        return statusMessage;
    }

    public void setStatusMessage(int statusMessage) {
        this.statusMessage = statusMessage;
    }

    public Integer getStatusColor() {
        return statusColor;
    }

    public void setStatusColor(Integer statusColor) {
        this.statusColor = statusColor;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }
}
