package org.smartregister.reveal.dao;

import com.vijay.jsonwizard.constants.JsonFormConstants;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.dao.AbstractDao;
import org.smartregister.reveal.util.Constants;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

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
        return (int) (Math.random() * 10);
    }

    public int getPZQReturned(String operationalAreaID) {
        String sql = "select json from event where eventType = '" + Constants.Events.DRUG_RETURNED + "'";

        AtomicInteger totalCount = new AtomicInteger(0);
        DataMap<Void> dataMap = cursor -> {
            String content = getCursorValue(cursor, "json");

            try {
                JSONObject json = new JSONObject(content);
                String location = json.getString("locationId");
                if (!StringUtils.equals(operationalAreaID, location)) return null;

                Map<String, String> values = readEventJsonValues(json);

                Integer returnedTins = parseInt(values.get("nPZQreturnedtins"));
                Integer returnedTinsQuantity = parseInt(values.get("nPZQreturnedtinsquantity"));
                Integer returnedTinsQuantityOther = parseInt(values.get("nPZQreturnedtinsquantityother"));
                Integer returnedTinsLoose = parseInt(values.get("nPZQreturnedloose"));

                int count = returnedTins * (returnedTinsQuantity != null ? returnedTinsQuantity : returnedTinsQuantityOther)
                        + (returnedTinsLoose != null ? returnedTinsLoose : 0);

                totalCount.set(totalCount.get() + count);

            } catch (JSONException e) {
                Timber.e(e);
            }
            return null;
        };

        readData(sql, dataMap);
        return totalCount.get();
    }

    private Integer parseInt(String value) {
        try {
            if (StringUtils.isNotBlank(value)) return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            Timber.e(e);
        }
        return null;
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

    public int getPZQReceived(String operationalAreaID) {
        String sql = "select json from event where eventType = '" + Constants.Events.DRUG_ALLOCATION + "'";

        AtomicInteger totalCount = new AtomicInteger(0);
        DataMap<Void> dataMap = cursor -> {
            String content = getCursorValue(cursor, "json");

            try {
                JSONObject json = new JSONObject(content);
                String location = json.getString("locationId");
                if (!StringUtils.equals(operationalAreaID, location)) return null;

                Map<String, String> values = readEventJsonValues(json);

                Integer receivedTins = parseInt(values.get("nPZQreceivedtins"));
                Integer receivedTinsSize = parseInt(values.get("nPZQreceivedtinsize"));
                Integer receivedTinsSizeOther = parseInt(values.get("nPZQreceivedtinsizeother"));
                Integer receivedTinsLoose = parseInt(values.get("nPZQreceivedloose"));

                int count = receivedTins * (receivedTinsSize != null ? receivedTinsSize : receivedTinsSizeOther)
                        + (receivedTinsLoose != null ? receivedTinsLoose : 0);

                totalCount.set(totalCount.get() + count);

            } catch (Exception e) {
                Timber.e(e);
            }
            return null;
        };

        readData(sql, dataMap);
        return totalCount.get();
    }

    public int getPZQDistributed(String operationalAreaID) {
        String sql = "select json from event where eventType = '" + Constants.Events.CHILD_DRUG_DISTRIBUTION + "'";

        AtomicInteger totalCount = new AtomicInteger(0);
        DataMap<Void> dataMap = cursor -> {
            String content = getCursorValue(cursor, "json");

            try {
                JSONObject json = new JSONObject(content);
                String location = json.getString("locationId");
                if (!StringUtils.equals(operationalAreaID, location)) return null;

                Map<String, String> values = readEventJsonValues(json);

                Integer receivedTins = parseInt(values.get("nPzqDistributedQuantity"));
                int count = receivedTins == null ? 0 : receivedTins;

                totalCount.set(totalCount.get() + count);

            } catch (Exception e) {
                Timber.e(e);
            }
            return null;
        };

        readData(sql, dataMap);
        return totalCount.get();
    }

    public int getTotalChildrenReceivedDrugs(String operationalAreaID) {
        return (int) (Math.random() * 10);
    }

    public int getTotalExpectedRegistrations(String operationalAreaID) {
        return (int) (Math.random() * 10);
    }

}
