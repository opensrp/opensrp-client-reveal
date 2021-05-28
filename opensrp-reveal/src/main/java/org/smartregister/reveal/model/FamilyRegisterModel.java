package org.smartregister.reveal.model;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.AllConstants;
import org.smartregister.clientandeventmodel.Obs;
import org.smartregister.domain.Location;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.model.BaseFamilyRegisterModel;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.repository.LocationRepository;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.Properties;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.util.JsonFormUtils;

import java.util.Arrays;
import java.util.List;

import timber.log.Timber;

import static org.smartregister.reveal.util.FamilyConstants.DatabaseKeys.COMPOUND_STRUCTURE;
import static org.smartregister.reveal.util.FamilyConstants.DatabaseKeys.FAMILY_NAME;
import static org.smartregister.reveal.util.FamilyConstants.RELATIONSHIP.RESIDENCE;
import static org.smartregister.util.JsonFormUtils.KEY;
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
            if(Country.NIGERIA.equals(BuildConfig.BUILD_COUNTRY) && eventClient.getEvent().getEntityType().equals(FamilyConstants.TABLE_NAME.FAMILY)){
                correctCompoundStructureFieldMultiSelectValue(eventClient, jsonString);
            }
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

        if(Country.NIGERIA.equals(BuildConfig.BUILD_COUNTRY)){
            populateCompoundStructureOptions(form);
        }
        return form;
    }

    public static void populateCompoundStructureOptions(JSONObject form){
        SQLiteDatabase database = RevealApplication.getInstance().getRepository().getReadableDatabase();
        Location operationalArea = RevealApplication.getInstance().getContext().getLocationRepository().getLocationByName(PreferencesUtil.getInstance().getCurrentOperationalArea());
        JSONObject property;
        JSONObject option;
        JSONArray options = new JSONArray();
        String query = String.format("SELECT %s,%s,%s FROM %s WHERE %s IS NULL AND %s IN (SELECT %s FROM %s WHERE %s = ? ) ORDER BY %s DESC",
                                Constants.DatabaseKeys.STRUCTURE_ID,
                                Constants.DatabaseKeys.FIRST_NAME,
                                Constants.DatabaseKeys.LAST_NAME,
                                FamilyConstants.TABLE_NAME.FAMILY,
                                FamilyConstants.DatabaseKeys.COMPOUND_STRUCTURE,
                                Constants.DatabaseKeys.STRUCTURE_ID,
                                Constants.DatabaseKeys.ID,
                                Constants.DatabaseKeys.STRUCTURES_TABLE,
                                Constants.DatabaseKeys.PARENT_ID,
                                Constants.DatabaseKeys.LAST_INTERACTED_WITH);
        try (Cursor cursor = database.rawQuery(query, new String[]{operationalArea.getId()})){
            while (cursor.moveToNext()) {

                property = new JSONObject();
                property.put("presumed-id","er");
                property.put("confirmed-id","er");

                String structureId = cursor.getString(cursor.getColumnIndex(Constants.DatabaseKeys.STRUCTURE_ID));
                String firsName = cursor.getString(cursor.getColumnIndex(Constants.DatabaseKeys.FIRST_NAME));
                String lastName = cursor.getString(cursor.getColumnIndex(Constants.DatabaseKeys.LAST_NAME));

                option = new JSONObject();
                option.put("key",structureId);
                option.put("text",String.format("%s %s",firsName,lastName));
                option.put("property",property);
                options.put(option);
            }

            JSONObject compoundStructureField = JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(form), FamilyConstants.FormKeys.COMPOUND_STRUCTURE);
            compoundStructureField.put(AllConstants.OPTIONS, options);
        }catch (JSONException e) {
                Timber.e(e, "Error populating %s Options",FamilyConstants.FormKeys.COMPOUND_STRUCTURE);
        } catch (Exception e) {
            Timber.e(e, "Error find Families ");
        }

    }

    private void correctCompoundStructureFieldMultiSelectValue(FamilyEventClient eventClient, String jsonString){
      try {
          String compoundStructure = JsonFormUtils.getFieldValue(jsonString,FamilyConstants.FormKeys.COMPOUND_STRUCTURE);
          if(compoundStructure != null){
              JSONObject correctValue = (JSONObject) new JSONArray(compoundStructure).get(0);
              eventClient.getEvent().addDetails(FamilyConstants.FormKeys.COMPOUND_STRUCTURE,correctValue.get(KEY).toString());
              for(Obs obs : eventClient.getEvent().getObs()){
                  if(obs.getFormSubmissionField().equals(FamilyConstants.FormKeys.COMPOUND_STRUCTURE)){
                      obs.setValues(Arrays.asList(correctValue.get(KEY).toString()));
                      obs.setHumanReadableValues(Arrays.asList(correctValue.get(KEY).toString()));
                      obs.setFieldCode(FamilyConstants.FormKeys.COMPOUND_STRUCTURE);
                      break;
                  }
              }
          }

      }catch (JSONException e){
         Timber.e(e);
      }

    }

    public String getStructureId() {
        return structureId;
    }
}
