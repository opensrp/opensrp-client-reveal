package org.smartregister.reveal.model;

/**
 * @author Vincent Karuri
 */

public class CardDetails {
    protected String status;
    private int statusMessage;
    private Integer statusColor;
    private String reason;
    private String comments;

    public CardDetails(String status) {
        this.status = status;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
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

    public String getComments() {
        return comments;
    }

    public void setComments(String comments) {
        this.comments = comments;
    }
}
