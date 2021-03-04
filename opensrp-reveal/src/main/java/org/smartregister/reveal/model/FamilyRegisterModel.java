package org.smartregister.reveal.model;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.domain.Location;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.model.BaseFamilyRegisterModel;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.Properties;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.util.JsonFormUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import timber.log.Timber;

import static com.vijay.jsonwizard.constants.JsonFormConstants.KEYS;
import static com.vijay.jsonwizard.constants.JsonFormConstants.VALUES;
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
        populateCompoundStructureOptions(form);
        return form;
    }

    private void populateCompoundStructureOptions(JSONObject form){
        SQLiteDatabase database = RevealApplication.getInstance().getRepository().getReadableDatabase();
        JSONArray keys = new JSONArray();
        JSONArray values = new JSONArray();
        Cursor cursor = null;
        try{
                String query = String.format("SELECT %s,%s,%s FROM %s WHERE %s IS NULL",Constants.DatabaseKeys.STRUCTURE_ID,Constants.DatabaseKeys.FIRST_NAME,Constants.DatabaseKeys.LAST_NAME, FamilyConstants.TABLE_NAME.FAMILY, FamilyConstants.DatabaseKeys.COMPOUND_STRUCTURE);
                cursor = database.rawQuery(query,new String[]{});
                while (cursor.moveToNext()) {
                    String structureId = cursor.getString(cursor.getColumnIndex(Constants.DatabaseKeys.STRUCTURE_ID));
                    String firsName = cursor.getString(cursor.getColumnIndex(Constants.DatabaseKeys.FIRST_NAME));
                    String lastName = cursor.getString(cursor.getColumnIndex(Constants.DatabaseKeys.LAST_NAME));
                    keys.put(structureId);
                    values.put(String.format("%s %s",firsName,lastName));
                }
            } catch (Exception e) {
                Timber.e(e, "Error find Families ");
            } finally {
                if (cursor != null)
                    cursor.close();
            }
            JSONObject compoundStructureField = JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(form),FamilyConstants.FormKeys.COMPOUND_STRUCTURE);
            try {
                compoundStructureField.put(KEYS,keys);
                compoundStructureField.put(VALUES, values);
            } catch (JSONException e) {
                Timber.e(e, "Error populating %s Options",FamilyConstants.FormKeys.COMPOUND_STRUCTURE);
            }
    }

    public String getStructureId() {
        return structureId;
    }
}
