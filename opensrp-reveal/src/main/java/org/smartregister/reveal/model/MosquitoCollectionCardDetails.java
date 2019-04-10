package org.smartregister.reveal.model;

/**
 * @author Vincent Karuri
 */
public class MosquitoCollectionCardDetails extends CardDetails {

    private String trapSetDate;
    private String trapFollowUpDate;

    public MosquitoCollectionCardDetails(String status, String trapSetDate, String trapFollowUpDate) {
        super(status);
        this.status = status;
        this.trapSetDate = trapSetDate;
        this.trapFollowUpDate = trapFollowUpDate;
    }

    public String getTrapSetDate() {
        return trapSetDate;
    }

    public void setTrapSetDate(String trapSetDate) {
        this.trapSetDate = trapSetDate;
    }

    public String getTrapFollowUpDate() {
        return trapFollowUpDate;
    }

    public void setTrapFollowUpDate(String trapFollowUpDate) {
        this.trapFollowUpDate = trapFollowUpDate;
    }
}
