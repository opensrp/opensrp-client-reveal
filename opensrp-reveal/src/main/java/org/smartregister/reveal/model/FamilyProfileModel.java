package org.smartregister.reveal.model;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.model.BaseFamilyProfileModel;
import org.smartregister.family.util.JsonFormUtils;
import org.smartregister.family.util.Utils;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.util.FormUtils;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.EVENT_TYPE_FIELD;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LAST_NAME;
import static org.smartregister.reveal.util.FamilyConstants.RELATIONSHIP.RESIDENCE;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class FamilyProfileModel extends BaseFamilyProfileModel {

    private String structureId;

    private FamilyEventClient eventClient;

    private CommonPersonObject familyHeadPersonObject;

    private String taskIdentifier;

    private String planIdentifier;

    public FamilyProfileModel(String familyName) {
        super(familyName);
    }

    @Override
    public FamilyEventClient processMemberRegistration(String jsonString, String familyBaseEntityId) {
        eventClient = super.processMemberRegistration(jsonString, familyBaseEntityId);
        setPlanAndTaskIdentifiers(familyBaseEntityId);
        tagEventClientDetails(eventClient);
        return eventClient;
    }

    @Override
    public FamilyEventClient processFamilyRegistrationForm(String jsonString, String familyBaseEntityId) {
        eventClient = super.processFamilyRegistrationForm(jsonString, familyBaseEntityId);
        if(org.smartregister.reveal.util.Utils.isCountryBuild(Country.NIGERIA)){
            setPlanAndTaskIdentifiers(familyBaseEntityId);
        }
        tagEventClientDetails(eventClient);
        return eventClient;
    }

    @Override
    public FamilyEventClient processUpdateMemberRegistration(String jsonString, String familyBaseEntityId) {
        eventClient = super.processUpdateMemberRegistration(jsonString, familyBaseEntityId);
        if(org.smartregister.reveal.util.Utils.isCountryBuild(Country.NIGERIA)){
            setPlanAndTaskIdentifiers(familyBaseEntityId);
        }
        tagEventClientDetails(eventClient);
        return eventClient;
    }

    private void tagEventClientDetails(FamilyEventClient eventClient) {
        if (eventClient == null)
            return;
        if (structureId != null) {
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
            eventClient.getEvent().addDetails(Constants.Properties.LOCATION_ID, structureId);
        }
        eventClient.getEvent().addDetails(Constants.Properties.APP_VERSION_NAME, BuildConfig.VERSION_NAME);
        eventClient.getEvent().setLocationId(org.smartregister.reveal.util.Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea()).getId());

        if(getPlanIdentifier() != null){
            eventClient.getEvent().addDetails(Constants.Properties.PLAN_IDENTIFIER,getPlanIdentifier());
        }
        if(getTaskIdentifier() !=null){
            eventClient.getEvent().addDetails(Constants.Properties.TASK_IDENTIFIER,getTaskIdentifier());
        }
    }

    public void setStructureId(String structureId) {
        this.structureId = structureId;
    }

    public String getTaskIdentifier() {
        return taskIdentifier;
    }

    public void setTaskIdentifier(String taskIdentifier) {
        this.taskIdentifier = taskIdentifier;
    }

    public String getPlanIdentifier() {
        return planIdentifier;
    }

    public void setPlanIdentifier(String planIdentifier) {
        this.planIdentifier = planIdentifier;
    }

    @Override
    public JSONObject getFormAsJson(String formName, String entityId, String currentLocationId) throws Exception {

        JSONObject form = FormUtils.getInstance(Utils.context().applicationContext()).getFormJson(formName);
        if (form == null) {
            return null;
        }
        form = JsonFormUtils.getFormAsJson(form, formName, entityId, currentLocationId);

        if (formName.equals(Utils.metadata().familyMemberRegister.formName)) {
            JsonFormUtils.updateJsonForm(form, familyHeadPersonObject.getColumnmaps().get(LAST_NAME));
        }

        return form;
    }

    public void setFamilyHeadPersonObject(CommonPersonObject familyHeadPersonObject) {
        this.familyHeadPersonObject = familyHeadPersonObject;
    }

    public CommonPersonObject getFamilyHeadPersonObject() {
        return familyHeadPersonObject;
    }

    private void setPlanAndTaskIdentifiers(String familyBaseEntityId) {
            JSONObject eventsByBaseEntityId = RevealApplication.getInstance().getContext().getEventClientRepository().getEventsByBaseEntityId(familyBaseEntityId);
            JSONArray events = eventsByBaseEntityId.optJSONArray("events");
            JSONObject familyRegistrationEvent = null;
            for (int i = 0; i < events.length(); i++) {
                try {
                    JSONObject event = events.getJSONObject(i);
                    String eventType = event.getString(EVENT_TYPE_FIELD);
                    if (eventType.equals(FamilyConstants.EventType.FAMILY_REGISTRATION)) {
                        familyRegistrationEvent = event;
                        break;
                    }
                } catch (JSONException e) {
                    Timber.e(e);
                }
            }
            if (familyRegistrationEvent != null) {
                try {
                    JSONObject details = familyRegistrationEvent.getJSONObject(Constants.DETAILS);
                    String planIdentifier = details.getString(Constants.Properties.PLAN_IDENTIFIER);
                    String taskIdentifier = details.getString(Constants.Properties.TASK_IDENTIFIER);
                    setPlanIdentifier(planIdentifier);
                    setTaskIdentifier(taskIdentifier);
                } catch (JSONException e) {
                    Timber.e(e);
                }
            }
    }
}
