package org.smartregister.reveal.model;

import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.model.BaseFamilyProfileModel;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.util.Constants;

import static org.smartregister.reveal.util.FamilyConstants.RELATIONSHIP.RESIDENCE;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class FamilyProfileModel extends BaseFamilyProfileModel {

    private String structureId;

    private FamilyEventClient eventClient;

    public FamilyProfileModel(String familyName) {
        super(familyName);
    }

    @Override
    public FamilyEventClient processMemberRegistration(String jsonString, String familyBaseEntityId) {
        eventClient = super.processMemberRegistration(jsonString, familyBaseEntityId);
        tagEventClientDetails(eventClient);
        return eventClient;
    }

    @Override
    public FamilyEventClient processFamilyRegistrationForm(String jsonString, String familyBaseEntityId) {
        eventClient = super.processFamilyRegistrationForm(jsonString, familyBaseEntityId);
        tagEventClientDetails(eventClient);
        return eventClient;
    }

    @Override
    public FamilyEventClient processUpdateMemberRegistration(String jsonString, String familyBaseEntityId) {
        eventClient = super.processUpdateMemberRegistration(jsonString, familyBaseEntityId);
        tagEventClientDetails(eventClient);
        return eventClient;
    }

    private void tagEventClientDetails(FamilyEventClient eventClient) {
        if (structureId != null) {
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
            eventClient.getEvent().addDetails(Constants.Properties.LOCATION_UUID, structureId);
        }
        eventClient.getEvent().addDetails(Constants.Properties.APP_VERSION_NAME, BuildConfig.VERSION_NAME);
    }

    public void setStructureId(String structureId) {
        this.structureId = structureId;
    }


    public FamilyEventClient getEventClient() {
        return eventClient;
    }
}
