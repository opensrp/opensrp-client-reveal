package org.smartregister.reveal.dao;

import com.google.common.util.concurrent.AtomicDouble;
import com.vijay.jsonwizard.constants.JsonFormConstants;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.dao.AbstractDao;
import org.smartregister.reveal.model.MDAOutCome;
import org.smartregister.reveal.util.Constants;

import java.util.HashMap;
import java.util.Map;

import timber.log.Timber;

public class ReportDao extends AbstractDao {

    public static ReportDao getInstance() {
        return new ReportDao();
    }


    public int getTotalStructures(String operationalAreaID) {
        String sql = "select count(*) cnt from structure where parent_id = '" + operationalAreaID + "'";
        DataMap<Integer> dataMap = cursor -> getCursorIntValue(cursor, "cnt");
        return readSingleValue(sql, dataMap, 0);
    }


    public int getTotalVisitedStructures(String operationalAreaID) {
        String sql = "select count(distinct task.structure_id) cnt from structure inner join task on task.structure_id = structure._id " +
                "where status = 'COMPLETED' and structure.parent_id = '" + operationalAreaID + "' ";
        DataMap<Integer> dataMap = cursor -> getCursorIntValue(cursor, "cnt");
        return readSingleValue(sql, dataMap, 0);
    }

    public double getPZQReturned(String operationalAreaID) {
        String sql = "select json from event where eventType = '" + Constants.Events.DRUG_RETURNED + "'";

        AtomicDouble totalCount = new AtomicDouble(0);
        DataMap<Void> dataMap = cursor -> {
            String content = getCursorValue(cursor, "json");

            try {
                JSONObject json = new JSONObject(content);
                String location = json.getString("locationId");
                if (!StringUtils.equals(operationalAreaID, location)) return null;

                Map<String, String> values = readEventJsonValues(json);

                double returnedTins = parseDouble(values.get("nPZQreturnedtins"));
                double returnedTinsQuantity = "Other".equalsIgnoreCase(values.get("nPZQreturnedtinsquantity")) ? 0 : parseDouble(values.get("nPZQreturnedtinsquantity"));
                double returnedTinsQuantityOther = parseDouble(values.get("nPZQreturnedtinsquantityother"));
                double returnedTinsLoose = parseDouble(values.get("nPZQreturnedloose"));

                double count = returnedTins * (returnedTinsQuantity > 0 ? returnedTinsQuantity : returnedTinsQuantityOther)
                        + returnedTinsLoose;

                totalCount.set(totalCount.get() + count);

            } catch (JSONException e) {
                Timber.e(e);
            }
            return null;
        };

        readData(sql, dataMap);
        return totalCount.get();
    }

    private double parseDouble(String value) {
        try {
            if (StringUtils.isNotBlank(value)) return Double.parseDouble(value);
        } catch (NumberFormatException e) {
            Timber.e(e);
        }
        return 0d;
    }

    private Map<String, String> readEventJsonValues(JSONObject json) throws JSONException {
        Map<String, String> values = new HashMap<>();

        JSONArray jsonArray = json.getJSONArray("obs");

        int index = 0;
        int count = jsonArray.length();

        while (index < count) {
            JSONObject jsonObject = jsonArray.getJSONObject(index);
            index++;

            if (!jsonObject.has(JsonFormConstants.VALUES) || !jsonObject.has("formSubmissionField"))
                continue;

            JSONArray objValues = jsonObject.getJSONArray(JsonFormConstants.VALUES);
            if (objValues.length() > 0)
                values.put(jsonObject.getString("formSubmissionField"), objValues.getString(0));
        }

        return values;
    }

    public double getPZQReceived(String operationalAreaID) {
        String sql = "select json from event where eventType = '" + Constants.Events.DRUG_ALLOCATION + "'";

        AtomicDouble totalCount = new AtomicDouble(0);
        DataMap<Void> dataMap = cursor -> {
            String content = getCursorValue(cursor, "json");

            try {
                JSONObject json = new JSONObject(content);
                String location = json.getString("locationId");
                if (!StringUtils.equals(operationalAreaID, location)) return null;

                Map<String, String> values = readEventJsonValues(json);

                double receivedTins = parseDouble(values.get("nPZQreceivedtins"));
                double receivedTinsSize = "Other".equalsIgnoreCase(values.get("nPZQreceivedtinsize")) ? 0 : parseDouble(values.get("nPZQreceivedtinsize"));
                double receivedTinsSizeOther = parseDouble(values.get("nPZQreceivedtinsizeother"));
                double receivedTinsLoose = parseDouble(values.get("nPZQreceivedloose"));

                double count = receivedTins * (receivedTinsSize > 0 ? receivedTinsSize : receivedTinsSizeOther)
                        + receivedTinsLoose;

                totalCount.set(totalCount.get() + count);

            } catch (Exception e) {
                Timber.e(e);
            }
            return null;
        };

        readData(sql, dataMap);
        return totalCount.get();
    }

    public double getPZQDistributed(String operationalAreaID) {
        String sql = "select json from event where eventType = '" + Constants.EventType.MDA_DISPENSE + "'";

        AtomicDouble totalCount = new AtomicDouble(0);
        DataMap<Void> dataMap = cursor -> {
            String content = getCursorValue(cursor, "json");

            try {
                JSONObject json = new JSONObject(content);
                String location = json.getString("locationId");
                if (!StringUtils.equals(operationalAreaID, location)) return null;

                Map<String, String> values = readEventJsonValues(json);

                double receivedTins = parseDouble(values.get("nPzqDistributedQuantity"));

                totalCount.set(totalCount.get() + receivedTins);

            } catch (Exception e) {
                Timber.e(e);
            }
            return null;
        };

        readData(sql, dataMap);
        return totalCount.get();
    }

    public int getTotalChildrenReceivedDrugs(String operationalAreaID) {
        String sql = "select sum(case when business_status = 'Drug Administered' then 1 else 0 end) completed " +
                "from task inner join ec_family_member on ec_family_member.base_entity_id = task.for " +
                "where code= '" + Constants.Intervention.NTD_MDA_DISPENSE + "' and ec_family_member.operational_area_id = '" + operationalAreaID + "'";
        DataMap<Integer> dataMap = cursor -> getCursorIntValue(cursor, "completed");
        return readSingleValue(sql, dataMap, 0);
    }

    public int getTotalExpectedRegistrations(String operationalAreaID) {
        String sql = "select ifnull(sum(nsac),0) expected_total from ec_family " +
                " where nsac is not null and trim(nsac) !=  '' and operational_area_id = '" + operationalAreaID + "'";
        DataMap<Integer> dataMap = cursor -> getCursorIntValue(cursor, "expected_total");
        return readSingleValue(sql, dataMap, 0);
    }

    public MDAOutCome calculateFamilyMDA(String familyBaseEntityId, String planId, String operationalId) {
        MDAOutCome result = new MDAOutCome();

        // total nsac
        String expectedTotal = "select ifnull(nsac,0) expected_total from ec_family " +
                "where nsac is not null and trim(nsac) !=  '' and base_entity_id = '" + familyBaseEntityId + "'";
        DataMap<Integer> expectedDataMap = cursor -> getCursorIntValue(cursor, "expected_total");

        result.setExpectedForms(readSingleValue(expectedTotal, expectedDataMap, 0));

        // get submissions
        String submissionsSql = "select count(*) total_tasks , sum(case when business_status = '" + Constants.BusinessStatus.VISITED_DRUG_ADMINISTERED + "' then 1 else 0 end) completed_tasks ,  " +
                " sum(case when business_status = '" + Constants.BusinessStatus.NOT_VISITED + "' then 1 else 0 end) not_visited " +
                "from task  " +
                "inner join ec_family_member on ec_family_member.base_entity_id = task.for " +
                "inner join ec_family on ec_family.base_entity_id = ec_family_member.relational_id " +
                "where code ='" + Constants.Intervention.NTD_MDA_DISPENSE + "' " +
                "and plan_id = '" + planId + "' and group_id = '" + operationalId + "' " +
                "and ec_family.base_entity_id = '" + familyBaseEntityId + "'";

        DataMap<Void> resultsDataMap = cursor -> {
            int totalTasks = getCursorIntValue(cursor, "total_tasks", 0);
            int completedTasks = getCursorIntValue(cursor, "completed_tasks", 0);
            int pendingVisits = getCursorIntValue(cursor, "not_visited", 0);

            if(result.getExpectedForms() < totalTasks)
                result.setExpectedForms(totalTasks);

            result.setNegativeForms(totalTasks - completedTasks - pendingVisits);
            result.setPositiveForms(completedTasks);
            return null;
        };

        readData(submissionsSql, resultsDataMap);

        return result;
    }

}
