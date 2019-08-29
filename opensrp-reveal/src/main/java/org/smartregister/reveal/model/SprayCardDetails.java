package org.smartregister.reveal.model;

import org.smartregister.commonregistry.CommonPersonObject;

/**
 * @author Vincent Karuri
 */
public class SprayCardDetails extends CardDetails {

    private String propertyType;
    private String sprayDate;
    private String sprayOperator;
    private String familyHead;
    private CommonPersonObject commonPersonObject;

    public SprayCardDetails(String status, String propertyType, String sprayDate, String sprayOperator, String familyHead, String reason) {
        super(status);
        this.propertyType = propertyType;
        this.sprayDate = sprayDate;
        this.sprayOperator = sprayOperator;
        this.familyHead = familyHead;
        setReason(reason);
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

    public CommonPersonObject getCommonPersonObject() {
        return commonPersonObject;
    }

    public void setCommonPersonObject(CommonPersonObject commonPersonObject) {
        this.commonPersonObject = commonPersonObject;
    }
}
