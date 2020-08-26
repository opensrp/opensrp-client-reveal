package org.smartregister.reveal.model;

/**
 * Created by Richard Kareko on 7/31/20.
 */

public class EventRegisterDetails {

    private String formSubmissionId;

    private String eventType;

    public String getFormSubmissionId() {
        return formSubmissionId;
    }

    public void setFormSubmissionId(String formSubmissionId) {
        this.formSubmissionId = formSubmissionId;
    }

    public String getEventType() {
        return eventType;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }
}
