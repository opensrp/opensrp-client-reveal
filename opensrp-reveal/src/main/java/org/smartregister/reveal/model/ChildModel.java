package org.smartregister.reveal.model;

import org.smartregister.dao.AbstractDao;
import org.smartregister.reveal.contract.ChildRegisterFragmentContract;
import org.smartregister.reveal.util.Constants;

import java.util.List;
import java.util.Map;

public class ChildModel extends AbstractDao implements ChildRegisterFragmentContract.Model {

    @Override
    public List<Child> filterChildren(Map<String, String> filterArgs, String sortArgs) {
        String query = "select * from ec_child";
        return AbstractDao.readData(query, getChildDataMap());
    }

    @Override
    public List<Child> searchChildren(String searchText, String sortArgs) {
        String query = "select * from ec_child";
        return AbstractDao.readData(query, getChildDataMap());
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
