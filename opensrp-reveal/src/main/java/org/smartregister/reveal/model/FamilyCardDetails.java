package org.smartregister.reveal.model;

public class FamilyCardDetails extends CardDetails {

    private String dateCreated;
    private String owner;

    public FamilyCardDetails(String status, String dateCreated, String owner) {
        super(status);
        this.dateCreated = dateCreated;
        this.owner = owner;
    }

    public String getDateCreated() {
        return dateCreated;
    }

    public void setDateCreated(String dateCreated) {
        this.dateCreated = dateCreated;
    }

    public String getOwner() {
        return owner;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }
}
