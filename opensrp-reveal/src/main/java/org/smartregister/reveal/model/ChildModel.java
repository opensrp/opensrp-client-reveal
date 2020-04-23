package org.smartregister.reveal.model;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.dao.AbstractDao;
import org.smartregister.reveal.contract.ChildRegisterFragmentContract;
import org.smartregister.reveal.util.Constants;
import org.smartregister.util.QueryComposer;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import timber.log.Timber;

public class ChildModel extends AbstractDao implements ChildRegisterFragmentContract.Model {

    @Override
    public List<Child> filterChildren(Map<String, String> filterArgs, String sortArgs) {
        QueryComposer composer = getDefaultComposer();

        if (StringUtils.isNotBlank(sortArgs))
            composer.withSortColumn(sortArgs);

        try {
            return AbstractDao.readData(composer.generateQuery(), getChildDataMap());
        } catch (QueryComposer.InvalidQueryException e) {
            Timber.e(e);
            return new ArrayList<>();
        }
    }

    @Override
    public List<Child> searchChildren(String searchText, String sortArgs) {
        QueryComposer composer = getDefaultComposer();

        if (StringUtils.isNotBlank(searchText))
            composer.withWhereClause(Constants.DatabaseKeys.FIRST_NAME + " like '%" + searchText + "%'");

        if (StringUtils.isNotBlank(sortArgs))
            composer.withSortColumn(sortArgs);

        try {
            return AbstractDao.readData(composer.generateQuery(), getChildDataMap());
        } catch (QueryComposer.InvalidQueryException e) {
            Timber.e(e);
            return new ArrayList<>();
        }
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
            return child;
        };
    }
}
