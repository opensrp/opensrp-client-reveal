package org.smartregister.reveal.util;

import android.support.v4.util.Pair;

import org.smartregister.domain.Task;

/**
 * Created by ndegwamartin on 2019-09-24.
 */
public class DBQueryHelper {

    /**
     * Gets the where clause for the task register, filters by operational area and campaign
     *
     * @return pair of filter clause and values for filter
     */
    public static Pair<String, String[]> getMainCondition() {
        org.smartregister.domain.Location operationalArea = org.smartregister.reveal.util.Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        String whereClause = String.format("%s.%s = ? AND %s.%s = ? AND %s.%s != ?",
                org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_TABLE, org.smartregister.reveal.util.Constants.DatabaseKeys.GROUPID, org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_TABLE, org.smartregister.reveal.util.Constants.DatabaseKeys.PLAN_ID,
                org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_TABLE, org.smartregister.reveal.util.Constants.DatabaseKeys.STATUS);
        return new Pair<>(whereClause, new String[]{operationalArea == null ?
                null : operationalArea.getId(), PreferencesUtil.getInstance().getCurrentPlanId(), Task.TaskStatus.CANCELLED.name()});
    }
}
