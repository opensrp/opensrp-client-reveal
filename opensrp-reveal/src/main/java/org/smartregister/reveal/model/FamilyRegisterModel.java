package org.smartregister.reveal.model;

import org.json.JSONObject;
import org.smartregister.domain.Location;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.model.BaseFamilyRegisterModel;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.util.Constants.Properties;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.util.JsonFormUtils;

import java.util.List;

import static org.smartregister.reveal.util.FamilyConstants.DatabaseKeys.FAMILY_NAME;
import static org.smartregister.reveal.util.FamilyConstants.RELATIONSHIP.RESIDENCE;
import static org.smartregister.util.JsonFormUtils.VALUE;

/**
 * Created by samuelgithengi on 4/10/19.
 */
public class FamilyRegisterModel extends BaseFamilyRegisterModel {

    private String structureId;
    private final String taskId;
    private final String taskBusinessStatus;
    private final String taskStatus;
    private final String structureName;


    public FamilyRegisterModel(String structureId, String taskId, String taskBusinessStatus, String taskStatus, String structureName) {
        this.structureId = structureId;
        this.taskId = taskId;
        this.taskBusinessStatus = taskBusinessStatus;
        this.taskStatus = taskStatus;
        this.structureName = structureName;
    }

    @Override
    public List<FamilyEventClient> processRegistration(String jsonString) {
        List<FamilyEventClient> eventClientList = super.processRegistration(jsonString);
        for (FamilyEventClient eventClient : eventClientList) {
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
            eventClient.getEvent().addDetails(Properties.TASK_IDENTIFIER, taskId);
            eventClient.getEvent().addDetails(Properties.TASK_BUSINESS_STATUS, taskBusinessStatus);
            eventClient.getEvent().addDetails(Properties.TASK_STATUS, taskStatus);
            eventClient.getEvent().addDetails(Properties.LOCATION_UUID, structureId);
            eventClient.getEvent().addDetails(Properties.APP_VERSION_NAME, BuildConfig.VERSION_NAME);
            String planIdentifier = PreferencesUtil.getInstance().getCurrentPlanId();
            eventClient.getEvent().addDetails(Properties.PLAN_IDENTIFIER, planIdentifier);
            Location operationalArea = org.smartregister.reveal.util.Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
            if (operationalArea != null)
                eventClient.getEvent().setLocationId(operationalArea.getId());
        }
        return eventClientList;
    }

    @Override
    public JSONObject getFormAsJson(String formName, String entityId, String currentLocationId) throws Exception {
        JSONObject form = super.getFormAsJson(formName, entityId, currentLocationId);
        JSONObject familyNameFieldJSONObject = JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(form), FAMILY_NAME);
        if (familyNameFieldJSONObject != null) {
            familyNameFieldJSONObject.put(VALUE, this.structureName);
        }
        return form;
    }

    public String getStructureId() {
        return structureId;
    }
}
