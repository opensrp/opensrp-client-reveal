package org.smartregister.reveal.model;

import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.model.BaseFamilyRegisterModel;
import org.smartregister.reveal.util.Constants;

import java.util.List;

import static org.smartregister.reveal.util.FamilyConstants.RELATIONSHIP.RESIDENCE;

/**
 * Created by samuelgithengi on 4/10/19.
 */
public class FamilyRegisterModel extends BaseFamilyRegisterModel {

    private String structureId;
    private final String taskId;
    private final String taskBusinessStatus;
    private final String taskStatus;

    public FamilyRegisterModel(String structureId, String taskId, String taskBusinessStatus, String taskStatus) {
        this.structureId = structureId;
        this.taskId = taskId;
        this.taskBusinessStatus = taskBusinessStatus;
        this.taskStatus = taskStatus;
    }

    @Override
    public List<FamilyEventClient> processRegistration(String jsonString) {
        List<FamilyEventClient> eventClientList = super.processRegistration(jsonString);
        for (FamilyEventClient eventClient : eventClientList) {
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
            eventClient.getEvent().addDetails(Constants.Properties.TASK_IDENTIFIER, taskId);
            eventClient.getEvent().addDetails(Constants.Properties.TASK_BUSINESS_STATUS, taskBusinessStatus);
            eventClient.getEvent().addDetails((Constants.Properties.TASK_STATUS, taskStatus);
            eventClient.getEvent().addDetails(Constants.Properties.LOCATION_UUID, structureId);
        }
        return eventClientList;
    }
}
