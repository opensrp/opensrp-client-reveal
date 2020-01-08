package org.smartregister.reveal.model;

/**
 * Created by samuelgithengi on 1/8/20.
 */
public class StructureDetails {

    private String structureName;

    private String familyMembersNames;

    public StructureDetails(String structureName, String familyMembersNames) {
        this.structureName = structureName;
        this.familyMembersNames = familyMembersNames;
    }

    public String getStructureName() {
        return structureName;
    }

    public void setStructureName(String structureName) {
        this.structureName = structureName;
    }

    public String getFamilyMembersNames() {
        return familyMembersNames;
    }

    public void setFamilyMembersNames(String familyMembersNames) {
        this.familyMembersNames = familyMembersNames;
    }
}
