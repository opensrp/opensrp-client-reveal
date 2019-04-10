package org.smartregister.reveal.model;

import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.model.BaseFamilyRegisterModel;

import java.util.List;

import static org.smartregister.reveal.util.FamilyConstants.RELATIONSHIP.RESIDENCE;

/**
 * Created by samuelgithengi on 4/10/19.
 */
public class FamilyRegisterModel extends BaseFamilyRegisterModel {

    private String structureId;

    public FamilyRegisterModel(String structureId) {
        this.structureId = structureId;
    }

    @Override
    public List<FamilyEventClient> processRegistration(String jsonString) {
        List<FamilyEventClient> eventClientList = super.processRegistration(jsonString);
        for (FamilyEventClient eventClient : eventClientList) {
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
        }
        return eventClientList;
    }
}
