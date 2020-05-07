package org.smartregister.reveal.model;

import android.content.Context;
import android.support.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.dao.AbstractDao;
import org.smartregister.repository.UniqueIdRepository;
import org.smartregister.reveal.contract.ChildRegisterFragmentContract;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.util.QueryComposer;
import org.smartregister.util.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import timber.log.Timber;

public class ChildModel extends AbstractDao implements ChildRegisterFragmentContract.Model {

    private RevealJsonFormUtils revealJsonFormUtils = new RevealJsonFormUtils();

    @Override
    public List<Child> searchAndFilter(String schoolID, @Nullable HashMap<String, List<String>> sortAndFilter, @Nullable String searchText) {
        QueryComposer composer = getDefaultComposer();
        if (StringUtils.isNotBlank(searchText))
            composer.withWhereClause(Constants.DatabaseKeys.FIRST_NAME + " like '%" + searchText + "%'");

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
    public JSONObject getMDAForm(Context context, String baseEntityID) throws JSONException {
        // read form and inject base id
        String jsonForm = Utils.readAssetContents(context, Constants.JsonForm.NTD_MASS_DRUG_ADMINISTRATION);
        return new JSONObject(jsonForm);
    }

    @Override
    public JSONObject getRegistrationForm(Context context) throws JSONException {
        String jsonForm = Utils.readAssetContents(context, Constants.JsonForm.NTD_CHILD_REGISTRATION);
        JSONObject jsonObject = new JSONObject(jsonForm);

        // inject unique id
        String uniqueID = new UniqueIdRepository().getNextUniqueId().getOpenmrsId();
        if (StringUtils.isBlank(uniqueID))
            throw new IllegalStateException("No local unique ID");

        revealJsonFormUtils.populateField(jsonObject, Constants.DatabaseKeys.UNIQUE_ID, uniqueID, "value");
        return jsonObject;
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
                        toCSV(paramsGrade) + ")");
            }

            List<String> paramsAge = sortAndFilter.get(Constants.ChildFilter.FILTER_AGE);
            if (paramsAge != null && paramsAge.size() > 0) {
                composer.withWhereClause(" cast(strftime('%Y.%m%d', 'now') - strftime('%Y.%m%d', " + Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.DOB + ") as int) in ( " +
                        toCSV(paramsAge) + ")");
            }
        }
    }

    private String toCSV(List<String> results) {
        StringBuilder builder = new StringBuilder();
        int size = results.size();
        while (size > 0) {
            builder.append(results.get(size - 1));
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
            return child;
        };
    }
}
