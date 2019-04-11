package org.smartregister.reveal.model;

/**
 * @author Vincent Karuri
 */
public class MosquitoHarvestCardDetails extends CardDetails {

    private String startDate;
    private String endDate;
    private String interventionType;

    public MosquitoHarvestCardDetails(String status, String trapSetDate, String endDate, String interventionType) {
        this.status = status;
        this.startDate = trapSetDate;
        this.endDate = endDate;
        this.interventionType = interventionType;
    }

    public String getStartDate() {
        return startDate;
    }

    public void setStartDate(String startDate) {
        this.startDate = startDate;
    }

    public String getEndDate() {
        return endDate;
    }

    public void setEndDate(String endDate) {
        this.endDate = endDate;
    }

    public String getInterventionType() {
        return interventionType;
    }

    public void setInterventionType(String interventionType) {
        this.interventionType = interventionType;
    }
}
