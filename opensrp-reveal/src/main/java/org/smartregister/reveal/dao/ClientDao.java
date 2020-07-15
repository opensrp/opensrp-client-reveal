package org.smartregister.reveal.dao;

import org.smartregister.dao.AbstractDao;

public class ClientDao extends AbstractDao {

    public static ClientDao getInstance() {
        return new ClientDao();
    }

    public String getClient(String baseEntityID) {
        String sql = "select json from client where baseEntityId = '" + baseEntityID + "'";
        return readSingleValue(sql, cursor -> getCursorValue(cursor, "json"));
    }
}
