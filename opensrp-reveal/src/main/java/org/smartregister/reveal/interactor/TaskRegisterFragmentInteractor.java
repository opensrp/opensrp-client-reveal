package org.smartregister.reveal.interactor;

import org.smartregister.cursoradapter.SmartRegisterQueryBuilder;

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
        return "select t._id,s.family_head_name name, t.\"for\" from " + tableName + " t " +
                "left join sprayed_structures s on t.\"for\"=s.base_entity_id WHERE " + mainCondition;
    }


}
