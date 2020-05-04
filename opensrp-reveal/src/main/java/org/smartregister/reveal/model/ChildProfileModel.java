package org.smartregister.reveal.model;

import android.content.Context;
import android.support.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.dao.AbstractDao;
import org.smartregister.reveal.contract.ChildProfileContract;
import org.smartregister.reveal.util.Constants;
import org.smartregister.util.QueryComposer;
import org.smartregister.util.Utils;

public class ChildProfileModel extends AbstractDao implements ChildProfileContract.Model {

    @Nullable
    @Override
    public Child getChild(String baseEntityID) throws QueryComposer.InvalidQueryException {
        QueryComposer composer = new QueryComposer()
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.BASE_ENTITY_ID)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.FIRST_NAME)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.LAST_NAME)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.DOB)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.MIDDLE_NAME)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.GENDER)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.GRADE)
                .withColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.UNIQUE_ID)

                .withMainTable(Constants.DatabaseKeys.CHILD_TABLE);

        composer.withWhereClause(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.BASE_ENTITY_ID + " = '" + baseEntityID + "'");

        DataMap<Child> dataMap = cursor -> {
            Child child = new Child();
            child.setBaseEntityID(getCursorValue(cursor, Constants.DatabaseKeys.BASE_ENTITY_ID));
            child.setFirstName(getCursorValue(cursor, Constants.DatabaseKeys.FIRST_NAME));
            child.setLastName(getCursorValue(cursor, Constants.DatabaseKeys.LAST_NAME));
            child.setBirthDate(getCursorValueAsDate(cursor, Constants.DatabaseKeys.DOB, getDobDateFormat()));
            child.setMiddleName(getCursorValue(cursor, Constants.DatabaseKeys.MIDDLE_NAME));
            child.setGender(getCursorValue(cursor, Constants.DatabaseKeys.GENDER));
            child.setGrade(getCursorValue(cursor, Constants.DatabaseKeys.GRADE));
            child.setUniqueID(getCursorValue(cursor, Constants.DatabaseKeys.UNIQUE_ID));
            if (child.getGrade() == null)
                child.setGrade("Grade " +
                        (StringUtils.isNotBlank(child.getFirstName()) ? child.getFirstName().substring(0, 1) : "Unknown")
                );
            return child;
        };

        return AbstractDao.readSingleValue(composer.generateQuery(), dataMap);
    }

    @Override
    public JSONObject getRegistrationEditForm(Context context, String baseEntityID) throws JSONException {
        String jsonForm = Utils.readAssetContents(context, Constants.JsonForm.NTD_CHILD_REGISTRATION);
        return new JSONObject(jsonForm);
    }
}
