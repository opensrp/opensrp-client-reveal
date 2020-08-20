package org.smartregister.reveal.dao;

import org.smartregister.dao.AbstractDao;

public class FamilyMemberDao extends AbstractDao {

    public static FamilyMemberDao getInstance() {
        return new FamilyMemberDao();
    }

    public boolean isFamilyHead(String familyMemberId) {
        String sql = "select count(*) cnt from ec_family where family_head = '" + familyMemberId + "'";
        DataMap<Integer> dataMap = cursor -> getCursorIntValue(cursor, "cnt");
        int total = readSingleValue(sql, dataMap, 0);
        return total > 0;
    }
}
