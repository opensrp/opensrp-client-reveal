package org.smartregister.reveal.model;

import org.smartregister.dao.AbstractDao;
import org.smartregister.reveal.contract.ChildFilterFragmentContract;
import org.smartregister.reveal.util.Constants;
import org.smartregister.util.QueryComposer;

import java.util.ArrayList;
import java.util.List;

import timber.log.Timber;

public class ChildFilterFragmentModel extends AbstractDao implements ChildFilterFragmentContract.Model {

    @Override
    public List<String> fetchUniqueGrades(String schoolID) {
        QueryComposer composer = new QueryComposer()
                .withColumn("distinct " + Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.GRADE)
                .withMainTable(Constants.DatabaseKeys.CHILD_TABLE)
                .withSortColumn(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.GRADE)
                .withWhereClause(Constants.DatabaseKeys.CHILD_TABLE + "." + Constants.DatabaseKeys.GRADE + " is not null");

        DataMap<String> dataMap = cursor -> getCursorValue(cursor, Constants.DatabaseKeys.GRADE);

        try {
            return AbstractDao.readData(composer.generateQuery(), dataMap);
        } catch (QueryComposer.InvalidQueryException e) {
            Timber.e(e);
            return new ArrayList<>();
        }
    }
}
