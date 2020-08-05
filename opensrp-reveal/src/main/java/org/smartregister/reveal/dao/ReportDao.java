package org.smartregister.reveal.dao;

import org.smartregister.dao.AbstractDao;

public final class ReportDao extends AbstractDao {

    public static ReportDao getInstance() {
        return new ReportDao();
    }


    public final int getTotalStructures(String operationalAreaID) {
        String sql = "select count(*) cnt from structure where parent_id = '" + operationalAreaID + "'";
        DataMap<Integer> dataMap = cursor -> getCursorIntValue(cursor, "cnt");
        return readSingleValue(sql, dataMap, 0);
    }


    public final int getTotalVisitedStructures(String operationalAreaID) {
        return (int) (Math.random()*10);
    }

    public final int getPZQReturned(String operationalAreaID) {
        return (int) (Math.random()*10);
    }

    public final int getPZQDistributed(String operationalAreaID) {
        return (int) (Math.random()*10);
    }

    public final int getTotalChildrenReceivedDrugs(String operationalAreaID) {
        return (int) (Math.random()*10);
    }

    public final int getTotalExpectedRegistrations(String operationalAreaID) {
        return (int) (Math.random()*10);
    }

}
