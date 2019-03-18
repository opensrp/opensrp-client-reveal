package org.smartregister.reveal.interactor;

import org.smartregister.cursoradapter.SmartRegisterQueryBuilder;
import org.smartregister.family.util.DBConstants;
import org.smartregister.reveal.util.Constants.DatabaseKeys;

/**
 * Created by samuelgithengi on 3/18/19.
 */
public class TaskRegisterFragmentInteractor {

    public String countSelect(String tableName, String mainCondition) {
        SmartRegisterQueryBuilder countQueryBuilder = new SmartRegisterQueryBuilder();
        countQueryBuilder.SelectInitiateMainTableCounts(tableName);
        return countQueryBuilder.mainCondition(mainCondition);
    }

    public String mainSelect(String tableName, String mainCondition) {
        SmartRegisterQueryBuilder queryBuilder = new SmartRegisterQueryBuilder();
        queryBuilder.SelectInitiateMainTable(tableName, mainColumns(tableName));
        queryBuilder.customJoin("LEFT JOIN " + DatabaseKeys.SPRAYED_STRUCTURES +
                " ON  " + tableName + "." + DatabaseKeys.FOR + " = " + DatabaseKeys.SPRAYED_STRUCTURES + "." + DBConstants.KEY.BASE_ENTITY_ID + " COLLATE NOCASE ");
        return queryBuilder.mainCondition(mainCondition);
    }

    private String[] mainColumns(String tableName) {
        return new String[]{
                tableName + "." + DatabaseKeys.ID,
                tableName + "." + DatabaseKeys.CODE,
                tableName + "." + DatabaseKeys.FOR,
                tableName + "." + DatabaseKeys.BUSINESS_STATUS,
                DatabaseKeys.SPRAYED_STRUCTURES + "." + DatabaseKeys.FAMILY_NAME,

        };
    }


}
