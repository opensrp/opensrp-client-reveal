package org.smartregister.reveal.util;

import androidx.annotation.NonNull;

import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.widgets.DatePickerFactory;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.util.Utils;

import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Generates a repeating group fields and loads then to form
 */
public class RepeatingGroupGenerator {
    private JSONObject step;
    private String repeatingGroupKey;
    private String uniqueKeyField;
    private List<HashMap<String, String>> storedValues;
    private Map<String, String> columnMap;
    private Set<String> fieldsWithoutRefreshLogic;
    private Set<String> fieldsWithoutSpecialViewValidation;
    private Set<String> hiddenFields;
    private Set<String> readOnlyFields;
    private String baseEntityId;
    //    private Map<String, List<Map<String, Object>>> rulesFileMap = new HashMap<>();
//    private String stepName; /*Uncomment this after native form lib upgrade*?

    /**
     * Argument stepName will be needed after native form lib upgrade
     */
    public RepeatingGroupGenerator(@NonNull JSONObject step,
            /*@NonNull String stepName,*/
                                   @NonNull String repeatingGroupKey,
                                   @NonNull Map<String, String> columnMap,
                                   @NonNull String uniqueKeyField,
                                   @NonNull List<HashMap<String, String>> storedValues) {
        this.repeatingGroupKey = repeatingGroupKey;
        this.uniqueKeyField = uniqueKeyField;
        this.storedValues = storedValues;
        this.step = step;
        this.columnMap = columnMap;
//        this.stepName = stepName;
    }

    public JSONObject getStep() {
        return step;
    }

    public void init() throws JSONException {
        JSONArray repeatingGrpValues = null;
        JSONArray stepFields = step.optJSONArray(JsonFormConstants.FIELDS);
        int pos = 0;
        for (int i = 0; i < stepFields.length(); i++) {
            JSONObject field = stepFields.optJSONObject(i);
            String key = field.optString(JsonFormConstants.KEY);
            if (key.equals(repeatingGroupKey)) {
                pos = i;
                repeatingGrpValues = field.optJSONArray(JsonFormConstants.VALUE);
                break;
            }
        }

        updateGroupValue(repeatingGrpValues, stepFields, pos);
    }

    private void updateGroupValue(JSONArray repeatingGrpValues, JSONArray stepFields, int pos) throws JSONException {
        int mPos = pos;
        for (Map<String, String> entryMap : storedValues) {
            baseEntityId = entryMap.get(uniqueKeyField);
            if (baseEntityId != null) {
                String baseEntityIdModified = baseEntityId.replaceAll("-", "");
                for (int i = 0; i < repeatingGrpValues.length(); i++) {
                    JSONObject object = repeatingGrpValues.optJSONObject(i);
                    JSONObject repeatingGrpField = new JSONObject(object.toString());
                    String repeatingGrpFieldKey = repeatingGrpField.optString(JsonFormConstants.KEY);

                    if (entryMap.containsKey(repeatingGrpFieldKey)) {
                        if (repeatingGrpField.optString(JsonFormConstants.TYPE).equals(JsonFormConstants.LABEL))
                            repeatingGrpField.put(JsonFormConstants.TEXT, processColumnValue(repeatingGrpFieldKey, entryMap.get(repeatingGrpFieldKey)));
                        else
                            repeatingGrpField.put(JsonFormConstants.VALUE, processColumnValue(repeatingGrpFieldKey, entryMap.get(repeatingGrpFieldKey)));
                    } else if (columnMap.get(repeatingGrpFieldKey) != null && entryMap.containsKey(columnMap.get(repeatingGrpFieldKey))) {
                        if (repeatingGrpField.optString(JsonFormConstants.TYPE).equals(JsonFormConstants.LABEL))
                            repeatingGrpField.put(JsonFormConstants.TEXT, processColumnValue(columnMap.get(repeatingGrpFieldKey), entryMap.get(columnMap.get(repeatingGrpFieldKey))));
                        else
                            repeatingGrpField.put(JsonFormConstants.VALUE, processColumnValue(columnMap.get(repeatingGrpFieldKey), entryMap.get(columnMap.get(repeatingGrpFieldKey))));
                    }

//                    updateFieldProperties(baseEntityIdModified, repeatingGrpField, repeatingGrpFieldKey);
                    updateFieldProperties(repeatingGrpField, repeatingGrpFieldKey);

                    updateField(repeatingGrpField, entryMap);
                    repeatingGrpField.put(JsonFormConstants.KEY, repeatingGrpFieldKey + "_" + baseEntityIdModified);
                    stepFields.put(++mPos, repeatingGrpField);
                }
            }
        }
    }

    /**
     * Argument baseEntityIdModified will be needed after native form lib upgrade
     */
    private void updateFieldProperties(
            // @NonNull String baseEntityIdModified,
            @NonNull JSONObject repeatingGrpField, @NonNull String repeatingGrpFieldKey) throws JSONException {
        if (readOnlyFields().contains(repeatingGrpFieldKey))
            repeatingGrpField.put(JsonFormConstants.READ_ONLY, "true");

        if (getFieldsWithoutRefreshLogic().contains(repeatingGrpFieldKey)) {
            repeatingGrpField.remove(JsonFormConstants.RELEVANCE);
            repeatingGrpField.remove(JsonFormConstants.CONSTRAINTS);
            repeatingGrpField.remove(JsonFormConstants.CALCULATION);
        }

//        update to optimized native forms
//        if (repeatingGrpField.has(JsonFormConstants.RELEVANCE) || repeatingGrpField.has(JsonFormConstants.CALCULATION)) {
//            generateDynamicRules(repeatingGrpField, baseEntityIdModified);
//        }

        if (getHiddenFields().contains(repeatingGrpFieldKey))
            repeatingGrpField.put(JsonFormConstants.TYPE, JsonFormConstants.HIDDEN);

        if (repeatingGrpFieldKey.equals(Constants.JsonForm.GENERATED_GRP))
            repeatingGrpField.put(JsonFormConstants.VALUE, "true");

        if (getFieldsWithoutSpecialViewValidation().contains(repeatingGrpFieldKey)) {
            repeatingGrpField.remove(JsonFormConstants.V_REQUIRED);
            repeatingGrpField.remove(JsonFormConstants.V_NUMERIC);
        }
    }
//    update to optimized native forms
//    protected void generateDynamicRules(@NonNull JSONObject field, @NonNull String uniqueId) {
//
//        try {
//            Context context = RevealApplication.getInstance().getContext().applicationContext();
//
//            com.vijay.jsonwizard.utils.Utils.buildRulesWithUniqueId(field, uniqueId, JsonFormConstants.RELEVANCE,
//                    context, rulesFileMap, stepName);
//
//            com.vijay.jsonwizard.utils.Utils.buildRulesWithUniqueId(field, uniqueId, JsonFormConstants.CALCULATION,
//                    context, rulesFileMap, stepName);
//
//            JSONObject relativeMaxValidator = field.optJSONObject(JsonFormConstants.V_RELATIVE_MAX);
//            if (relativeMaxValidator != null) {
//                String currRelativeMaxValidatorValue = relativeMaxValidator.getString(JsonFormConstants.VALUE);
//                String newRelativeMaxValidatorValue = currRelativeMaxValidatorValue + "_" + uniqueId;
//                relativeMaxValidator.put(JsonFormConstants.VALUE, newRelativeMaxValidatorValue);
//            }
//        } catch (JSONException e) {
//            Timber.e(e);
//        }
//    }

    public void updateField(JSONObject repeatingGrpField, Map<String, String> entryMap) throws JSONException {
        String type = repeatingGrpField.optString(JsonFormConstants.TYPE);
        if (type.equals(JsonFormConstants.CHECK_BOX) || type.equals(JsonFormConstants.NATIVE_RADIO_BUTTON) || type.equals(JsonFormConstants.SPINNER)) {
            repeatingGrpField.put(JsonFormConstants.VALUE, repeatingGrpField.optString(JsonFormConstants.VALUE).toLowerCase());
        }
    }

    public String processColumnValue(String columnName, String value) {
        String s = "";
        if (columnName.equals("dob")) {
            Date dob = Utils.dobStringToDate(value);
            if (dob != null) {
                s = DatePickerFactory.DATE_FORMAT.format(dob);
            }
        } else {
            s = value;
        }
        return s;
    }

    public Set<String> readOnlyFields() {
        if (readOnlyFields == null) {
            readOnlyFields = new HashSet<>();
        }
        return readOnlyFields;
    }

    public void setReadOnlyFields(Set<String> readOnlyFields) {
        this.readOnlyFields = readOnlyFields;
    }

    public Set<String> getFieldsWithoutRefreshLogic() {
        if (fieldsWithoutRefreshLogic == null) {
            fieldsWithoutRefreshLogic = new HashSet<>(Arrays.asList(""));
        }
        return fieldsWithoutRefreshLogic;
    }


    public void setFieldsWithoutRefreshLogic(Set<String> fieldsWithoutRefreshLogic) {
        this.fieldsWithoutRefreshLogic = fieldsWithoutRefreshLogic;
    }


    public Set<String> getFieldsWithoutSpecialViewValidation() {
        if (fieldsWithoutSpecialViewValidation == null) {
            fieldsWithoutSpecialViewValidation = new HashSet<>(Arrays.asList(""));
        }
        return fieldsWithoutSpecialViewValidation;
    }

    public void setFieldsWithoutSpecialViewValidation(Set<String> fieldsWithoutSpecialViewValidation) {
        this.fieldsWithoutSpecialViewValidation = fieldsWithoutSpecialViewValidation;
    }

    public void setHiddenFields(Set<String> hiddenFields) {
        this.hiddenFields = hiddenFields;
    }

    public Set<String> getHiddenFields() {
        if (hiddenFields == null) {
            hiddenFields = new HashSet<>(Arrays.asList(""));
        }
        return hiddenFields;
    }

    public String getBaseEntityId() {
        return baseEntityId;
    }
}
