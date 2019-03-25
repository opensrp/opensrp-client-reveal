package org.smartregister.reveal.util;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

import com.mapbox.geojson.Feature;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.domain.Location;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.activity.RevealJsonFormActivity;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Constants.JsonForm;
import org.smartregister.util.AssetHandler;

import static com.vijay.jsonwizard.constants.JsonFormConstants.KEY;
import static org.smartregister.reveal.util.Constants.DETAILS;
import static org.smartregister.reveal.util.Constants.ENTITY_ID;
import static org.smartregister.reveal.util.Constants.JSON_FORM_PARAM_JSON;
import static org.smartregister.reveal.util.Constants.JsonForm.HEAD_OF_HOUSEHOLD;
import static org.smartregister.reveal.util.Constants.JsonForm.SPRAY_FORM;
import static org.smartregister.reveal.util.Constants.JsonForm.SPRAY_FORM_BOTSWANA;
import static org.smartregister.reveal.util.Constants.JsonForm.SPRAY_FORM_NAMIBIA;
import static org.smartregister.reveal.util.Constants.JsonForm.SPRAY_STATUS;
import static org.smartregister.reveal.util.Constants.JsonForm.STRUCTURE_PROPERTIES_TYPE;
import static org.smartregister.reveal.util.Constants.JsonForm.STRUCTURE_TYPE;
import static org.smartregister.reveal.util.Constants.MOSQUITO_COLLECTION_EVENT;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_TYPE;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_UUID;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_VERSION;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.Properties.TASK_STATUS;
import static org.smartregister.reveal.util.Constants.REQUEST_CODE_GET_JSON;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;
import static org.smartregister.reveal.util.Utils.getPropertyValue;

/**
 * Created by samuelgithengi on 3/22/19.
 */
public class RevealJsonFormUtils {

    private static final String TAG = "RevealJsonFormUtils";


    public JSONObject getFormJSON(Context context, String formName, Feature feature, String sprayStatus, String familyHead) {

        String taskBusinessStatus = getPropertyValue(feature, TASK_BUSINESS_STATUS);
        String taskIdentifier = getPropertyValue(feature, TASK_IDENTIFIER);
        String taskStatus = getPropertyValue(feature, TASK_STATUS);

        String structureId = feature.id();
        String structureUUID = getPropertyValue(feature, LOCATION_UUID);
        String structureVersion = getPropertyValue(feature, LOCATION_VERSION);
        String structureType = getPropertyValue(feature, LOCATION_TYPE);

        String formString = getFormString(context, formName, structureType);
        try {
            JSONObject formJson = populateFormDetails(formString, structureId, taskIdentifier,
                    taskBusinessStatus, taskStatus, structureUUID,
                    structureVersion == null ? null : Integer.valueOf(structureVersion));

            populateFormFields(formJson, structureType, sprayStatus, familyHead);
            return formJson;
        } catch (Exception e) {
            Log.e(TAG, "error launching form" + formName, e);
        }
        return null;
    }

    public JSONObject getFormJSON(Context context, String formName, TaskDetails task, Location structure) {

        String taskBusinessStatus = task.getBusinessStatus();
        String taskIdentifier = task.getTaskId();
        String taskStatus = task.getTaskStatus();

        String structureId = structure.getId();
        String structureUUID = structure.getProperties().getUid();
        int structureVersion = structure.getProperties().getVersion();
        String structureType = structure.getProperties().getType();

        String sprayStatus = task.getSprayStatus();
        String familyHead = task.getFamilyName();

        String formString = getFormString(context, formName, structureType);
        try {
            JSONObject formJson = populateFormDetails(formString, structureId, taskIdentifier,
                    taskBusinessStatus, taskStatus, structureUUID, structureVersion);
            populateFormFields(formJson, structureType, sprayStatus, familyHead);
            return formJson;
        } catch (JSONException e) {
            Log.e(TAG, "error launching form" + formName, e);
        }
        return null;
    }

    private String getFormString(Context context, String formName, String structureType) {
        String formString = AssetHandler.readFileFromAssetsFolder(formName, context);
        if ((SPRAY_FORM.equals(formName) || SPRAY_FORM_BOTSWANA.equals(formName) || SPRAY_FORM_NAMIBIA.equals(formName))) {
            String structType = structureType;
            if (StringUtils.isBlank(structureType)) {
                structType = Constants.StructureType.NON_RESIDENTIAL;
            }
            formString = formString.replace(STRUCTURE_PROPERTIES_TYPE, structType);
        }
        return formString;
    }


    private JSONObject populateFormDetails(String formString, String structureId, String taskIdentifier,
                                           String taskBusinessStatus, String taskStatus, String structureUUID,
                                           Integer structureVersion) throws JSONException {

        JSONObject formJson = new JSONObject(formString);
        formJson.put(ENTITY_ID, structureId);
        JSONObject formData = new JSONObject();
        formData.put(TASK_IDENTIFIER, taskIdentifier);
        formData.put(TASK_BUSINESS_STATUS, taskBusinessStatus);
        formData.put(TASK_STATUS, taskStatus);
        formData.put(LOCATION_UUID, structureUUID);
        formData.put(LOCATION_VERSION, structureVersion);
        formJson.put(DETAILS, formData);
        return formJson;
    }


    private void populateFormFields(JSONObject formJson, String structureType, String sprayStatus, String familyHead) throws JSONException {

        JSONArray fields = org.smartregister.util.JsonFormUtils.fields(formJson);
        if (StringUtils.isNotBlank(structureType) || StringUtils.isNotBlank(sprayStatus) || StringUtils.isNotBlank(familyHead)) {
            for (int i = 0; i < fields.length(); i++) {
                JSONObject field = fields.getJSONObject(i);
                String key = field.getString(KEY);
                if (key.equalsIgnoreCase(STRUCTURE_TYPE))
                    field.put(org.smartregister.util.JsonFormUtils.VALUE, structureType);
                else if (key.equalsIgnoreCase(SPRAY_STATUS))
                    field.put(org.smartregister.util.JsonFormUtils.VALUE, sprayStatus);
                else if (key.equalsIgnoreCase(HEAD_OF_HOUSEHOLD))
                    field.put(org.smartregister.util.JsonFormUtils.VALUE, familyHead);
            }
        }

    }


    public void startJsonForm(JSONObject form, Activity context) {
        Intent intent = new Intent(context.getApplicationContext(), RevealJsonFormActivity.class);
        try {
            intent.putExtra(JSON_FORM_PARAM_JSON, form.toString());
            context.startActivityForResult(intent, REQUEST_CODE_GET_JSON);
        } catch (Exception e) {
            Log.e(TAG, e.getMessage());
        }
    }

    public static String getFormName(String encounterType, String taskCode) {
        String formName = null;
        if (SPRAY_EVENT.equals(encounterType) || Intervention.IRS.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.NAMIBIA) {
                formName = JsonForm.SPRAY_FORM_NAMIBIA;
            } else if (BuildConfig.BUILD_COUNTRY == Country.BOTSWANA) {
                formName = JsonForm.SPRAY_FORM_BOTSWANA;
            } else if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
                formName = JsonForm.SPRAY_FORM;
            }
        } else if (MOSQUITO_COLLECTION_EVENT.equals(encounterType)
                || Intervention.MOSQUITO_COLLECTION.equals(taskCode)) {
            formName = JsonForm.THAILAND_MOSQUITO_COLLECTION_FORM;
        }
        return formName;
    }
}
