package org.smartregister.reveal.model;

import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.model.BaseFamilyProfileModel;
import org.smartregister.reveal.util.AppExecutors;

import static org.smartregister.reveal.util.FamilyConstants.RELATIONSHIP.RESIDENCE;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class FamilyProfileModel extends BaseFamilyProfileModel {

    private String structureId;

    public FamilyProfileModel(String familyName) {
        super(familyName);
    }

    @Override
    public FamilyEventClient processMemberRegistration(String jsonString, String familyBaseEntityId) {
        FamilyEventClient eventClient = super.processMemberRegistration(jsonString, familyBaseEntityId);
        if (structureId != null)
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
        return eventClient;
    }

    @Override
    public FamilyEventClient processFamilyRegistrationForm(String jsonString, String familyBaseEntityId) {
        FamilyEventClient eventClient = super.processFamilyRegistrationForm(jsonString, familyBaseEntityId);
        if (structureId != null)
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
        return eventClient;
    }

    @Override
    public FamilyEventClient processUpdateMemberRegistration(String jsonString, String familyBaseEntityId) {
        FamilyEventClient eventClient = super.processUpdateMemberRegistration(jsonString, familyBaseEntityId);
        if (structureId != null)
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
        return eventClient;
    }

    public void setStructureId(String structureId) {
        this.structureId = structureId;
    }
}
