package org.smartregister.reveal.model;

import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.model.BaseFamilyRegisterModel;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.Properties;

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

    private  List<FamilyEventClient> eventClientList;

    public FamilyRegisterModel(String structureId, String taskId, String taskBusinessStatus, String taskStatus) {
        this.structureId = structureId;
        this.taskId = taskId;
        this.taskBusinessStatus = taskBusinessStatus;
        this.taskStatus = taskStatus;
    }

    @Override
    public List<FamilyEventClient> processRegistration(String jsonString) {
        eventClientList = super.processRegistration(jsonString);
        for (FamilyEventClient eventClient : eventClientList) {
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
            eventClient.getEvent().addDetails(Properties.TASK_IDENTIFIER, taskId);
            eventClient.getEvent().addDetails(Properties.TASK_BUSINESS_STATUS, taskBusinessStatus);
            eventClient.getEvent().addDetails(Properties.TASK_STATUS, taskStatus);
            eventClient.getEvent().addDetails(Properties.LOCATION_UUID, structureId);
            eventClient.getEvent().addDetails(Properties.APP_VERSION_NAME, BuildConfig.VERSION_NAME);
        }
        return eventClientList;
    }

    public List<FamilyEventClient> getEventClientList() {
        return eventClientList;
    }

    public String getStructureId() {
        return structureId;
    }
}
