package org.smartregister.reveal.model;

public class FamilySummaryModel {
    private int childrenTreated = 0;
    private int additionalDosesAdministered = 0;

    public int getAdditionalDosesAdministered() {
        return additionalDosesAdministered;
    }

    public void setAdditionalDosesAdministered(int additionalDosesAdministered) {
        this.additionalDosesAdministered = additionalDosesAdministered;
    }

    public int getChildrenTreated() {
        return childrenTreated;
    }

    public void setChildrenTreated(int childrenTreated) {
        this.childrenTreated = childrenTreated;
    }
}
