package org.smartregister.reveal.model;

import android.content.Context;
import android.support.annotation.Nullable;

import com.vijay.jsonwizard.constants.JsonFormConstants;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.CoreLibrary;
import org.smartregister.dao.AbstractDao;
import org.smartregister.domain.Task;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.ChildRegisterFragmentContract;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.JsonClientProcessingUtils;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.util.QueryComposer;
import org.smartregister.util.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import timber.log.Timber;

public class ChildModel extends AbstractDao implements ChildRegisterFragmentContract.Model {

    private RevealJsonFormUtils revealJsonFormUtils = new RevealJsonFormUtils();

    @Override
    public List<Child> searchAndFilter(@Nullable HashMap<String, List<String>> sortAndFilter, @Nullable String searchText) {
        QueryComposer composer = getDefaultComposer();
        if (StringUtils.isNotBlank(searchText))
            composer.withWhereClause(Constants.DatabaseKeys.FIRST_NAME + " like '%" + searchText + "%' or " +
                    Constants.DatabaseKeys.LAST_NAME + " like '%" + searchText + "%' or " +
                    Constants.DatabaseKeys.UNIQUE_ID + " like '%" + searchText + "%' "
            );

        String currentArea = getCurrentLocationID();
        if (StringUtils.isNotBlank(currentArea))
            composer.withWhereClause(Constants.DatabaseKeys.LOCATION + " = '" + currentArea + "'");


        extractSort(composer, sortAndFilter);
        extractFilter(composer, sortAndFilter);

        try {
            return AbstractDao.readData(composer.generateQuery(), getChildDataMap());
        } catch (QueryComposer.InvalidQueryException e) {
            Timber.e(e);
            return new ArrayList<>();
        }
    }

    @Override
    public String getCurrentLocationID() {
        return JsonClientProcessingUtils.getCurrentLocationID();
    }

    @Override
    public Map<String, Integer> getReportCounts() {
        Map<String, Integer> result = new HashMap<>();

        int registeredChildren = getTotal("select count(*) cnt from ec_child where DATE(dob) > DATE('now','-15 years') and is_closed = 0 ");

        int administeredDrugs = getTotal("select count(*) cnt from ec_child where DATE(dob) > DATE('now','-15 years') and is_closed = 0 " +
                "and ec_child.base_entity_id in (select t.for from task  t where t.code = 'MDA Dispense' and t.business_status = 'Visited, Drug Administered') ");

        int childrenNotVisited = getTotal("select count(*) cnt from ec_child where DATE(dob) > DATE('now','-15 years') and is_closed = 0 " +
                "and ec_child.base_entity_id not in (select baseEntityId from event where eventType in 'mda_dispense') ");

        int visitedNotAdministered = getTotal("select count(*) cnt from ec_child where DATE(dob) > DATE('now','-15 years') and is_closed = 0 " +
                "and ec_child.base_entity_id in (select t.for from task  t where t.code = 'MDA Dispense' and t.business_status = 'Visited, Drug Not Administered') ");

        result.put(Constants.ChildRegister.MMA_COVERAGE, registeredChildren == 0 ? 0 : (administeredDrugs / registeredChildren));
        result.put(Constants.ChildRegister.MMA_TARGET_REMAINING, (int) ((registeredChildren * 0.9) - administeredDrugs));
        result.put(Constants.ChildRegister.MMA_NOT_VISITED, childrenNotVisited);
        result.put(Constants.ChildRegister.MMA_VISITED_NOT_ADMINISTERED, visitedNotAdministered);

        return result;
    }

    private int getTotal(String sql) {
        DataMap<Integer> countMap = cursor -> getCursorIntValue(cursor, "cnt");
        Integer registeredChildren = AbstractDao.readSingleValue(sql, countMap);
        return registeredChildren == null ? 0 : registeredChildren;
    }

    @Override
    public JSONObject getMDAForm(Context context, String baseEntityID) throws JSONException {
        // read form and inject base id
        String jsonForm = Utils.readAssetContents(context, Constants.JsonForm.NTD_MASS_DRUG_ADMINISTRATION);
        JSONObject jsonObject = new JSONObject(jsonForm);
        jsonObject.put(Constants.Properties.BASE_ENTITY_ID, baseEntityID);
        return jsonObject;
    }

    @Override
    public JSONObject getRegistrationForm(Context context) throws JSONException {
        String jsonForm = Utils.readAssetContents(context, Constants.JsonForm.NTD_CHILD_REGISTRATION);
        JSONObject jsonObject = new JSONObject(jsonForm);

        // inject unique id
        String uniqueID = CoreLibrary.getInstance().context().getUniqueIdRepository().getNextUniqueId().getOpenmrsId();
        if (StringUtils.isBlank(uniqueID))
            throw new IllegalStateException("No local unique ID");

        revealJsonFormUtils.populateField(jsonObject, Constants.DatabaseKeys.UNIQUE_ID, uniqueID, JsonFormConstants.VALUE);
        return jsonObject;
    }

    @Override
    public Task getCurrentTask(Context context, String baseEntityID) {
        String taskSQL = "select _id from task where for = '" + baseEntityID + "' and code = '" + Constants.Intervention.MDA_DISPENSE + "' order by  authored_on desc limit 1";
        DataMap<String> dataMap = cursor -> getCursorValue(cursor, "_id");

        String taskId = AbstractDao.readSingleValue(taskSQL, dataMap);
        return RevealApplication.getInstance().getContext().getTaskRepository().getTaskByIdentifier(taskId);
    }

    private void extractSort(QueryComposer composer, @Nullable HashMap<String, List<String>> sortAndFilter) {
        if (sortAndFilter != null) {
            List<String> params = sortAndFilter.get(Constants.ChildFilter.SORT);
            if (params != null && params.size() > 0) {
                for (String s : params) {
                    composer.withSortColumn(s);
                }
            }
        }
    }

    private void extractFilter(QueryComposer composer, @Nullable HashMap<String, List<String>> sortAndFilter) {
        if (sortAndFilter != null) {
            List<String> paramsGrade = sortAndFilter.get(Constants.ChildFilter.FILTER_GRADE);
            if (paramsGrade != null && paramsGrade.size() > 0) {
                composer.withWhereClause(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.GRADE + " in ( " +
                        toCSV(paramsGrade, true) + ")");
            }

            List<String> paramsAge = sortAndFilter.get(Constants.ChildFilter.FILTER_AGE);
            if (paramsAge != null && paramsAge.size() > 0) {
                composer.withWhereClause(" cast(strftime('%Y.%m%d', 'now') - strftime('%Y.%m%d', " + Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.DOB + ") as int) in ( " +
                        toCSV(paramsAge, false) + ")");
            }
        }
    }

    private String toCSV(List<String> results, boolean textQualifier) {
        StringBuilder builder = new StringBuilder();
        int size = results.size();
        while (size > 0) {

            if (textQualifier)
                builder.append("'");

            builder.append(results.get(size - 1));

            if (textQualifier)
                builder.append("'");

            if (size > 1)
                builder.append(" , ");
            size--;
        }
        return builder.toString();
    }

    private QueryComposer getDefaultComposer() {
        return new QueryComposer()
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.BASE_ENTITY_ID)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.FIRST_NAME)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.LAST_NAME)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.DOB)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.MIDDLE_NAME)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.GENDER)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.GRADE)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.UNIQUE_ID)
                .withColumn(
                        "(select business_status from " + Constants.DatabaseKeys.TASK_TABLE + " where for = " +
                                Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.BASE_ENTITY_ID + " and code = '" +
                                Constants.Intervention.MDA_DISPENSE + "' order by authored_on desc limit 1) as " + Constants.DatabaseKeys.STATUS
                )

                .withMainTable(Constants.DatabaseKeys.CHILD_TABLE);
    }

    private DataMap<Child> getChildDataMap() {
        return cursor -> {
            Child child = new Child();
            child.setBaseEntityID(getCursorValue(cursor, Constants.DatabaseKeys.BASE_ENTITY_ID));
            child.setFirstName(getCursorValue(cursor, Constants.DatabaseKeys.FIRST_NAME));
            child.setLastName(getCursorValue(cursor, Constants.DatabaseKeys.LAST_NAME));
            child.setBirthDate(getCursorValueAsDate(cursor, Constants.DatabaseKeys.DOB, getDobDateFormat()));
            child.setMiddleName(getCursorValue(cursor, Constants.DatabaseKeys.MIDDLE_NAME));
            child.setGender(getCursorValue(cursor, Constants.DatabaseKeys.GENDER));
            child.setGrade(getCursorValue(cursor, Constants.DatabaseKeys.GRADE));
            child.setUniqueID(getCursorValue(cursor, Constants.DatabaseKeys.UNIQUE_ID));
            child.setTaskStatus(getCursorValue(cursor, Constants.DatabaseKeys.STATUS));
            if (child.getGrade() == null)
                child.setGrade("");

            return child;
        };
    }
}
