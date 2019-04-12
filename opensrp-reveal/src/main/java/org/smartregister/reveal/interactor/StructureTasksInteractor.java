package org.smartregister.reveal.interactor;

import android.util.Log;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.apache.commons.lang3.ArrayUtils;
import org.smartregister.cursoradapter.SmartRegisterQueryBuilder;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.Constants.DatabaseKeys;

import java.util.ArrayList;
import java.util.List;

import static org.smartregister.family.util.Constants.JSON_FORM_KEY.AGE;
import static org.smartregister.family.util.DBConstants.KEY.FIRST_NAME;
import static org.smartregister.family.util.DBConstants.KEY.LAST_NAME;
import static org.smartregister.family.util.DBConstants.KEY.MIDDLE_NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CODE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FOR;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_TABLE;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class StructureTasksInteractor implements StructureTasksContract.Interactor {


    private static final String TAG = StructureTasksInteractor.class.getName();
    private AppExecutors appExecutors;

    private SQLiteDatabase database;
    private StructureTasksContract.Presenter presenter;

    public StructureTasksInteractor(StructureTasksContract.Presenter presenter) {
        this.presenter = presenter;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        database = RevealApplication.getInstance().getRepository().getReadableDatabase();
    }

    @Override
    public void findTasks(String structureId) {
        appExecutors.diskIO().execute(() -> {
            List<StructureTaskDetails> taskDetailsList = new ArrayList<>();
            Cursor cursor = null;
            try {
                cursor = database.rawQuery(getStructureSelect(FOR + "=?"), new String[]{structureId});
                while (cursor.moveToNext()) {
                    taskDetailsList.add(readTaskDetails(cursor));
                }
            } catch (Exception e) {
                Log.e(TAG, "Error querying tasks for " + structureId, e);
            } finally {
                if (cursor != null) {
                    cursor.close();
                }
            }
            appExecutors.mainThread().execute(() -> {
                presenter.onTasksFound(taskDetailsList);
            });
        });
    }

    private String[] getFamilyColumns() {
        String[] columns = getStructureColumns();
        String names = "printf('%s %s %s'," + FIRST_NAME + "," + MIDDLE_NAME + "," + LAST_NAME + ")";
        ArrayUtils.add(columns, names);
        ArrayUtils.add(columns, AGE);
        return columns;
    }


    private String getStructureSelect(String mainCondition) {
        SmartRegisterQueryBuilder queryBuilder = new SmartRegisterQueryBuilder();
        queryBuilder.selectInitiateMainTable(DatabaseKeys.TASK_TABLE, getStructureColumns(), ID);
        return queryBuilder.mainCondition(mainCondition);
    }


    private String getMembersSelect(String mainCondition) {
        SmartRegisterQueryBuilder queryBuilder = new SmartRegisterQueryBuilder();
        queryBuilder.selectInitiateMainTable(DatabaseKeys.TASK_TABLE, getStructureColumns(), ID);
        return queryBuilder.mainCondition(mainCondition);
    }

    private String[] getStructureColumns() {
        return new String[]{
                TASK_TABLE + "." + ID,
                TASK_TABLE + "." + CODE,
                TASK_TABLE + "." + FOR,
                TASK_TABLE + "." + BUSINESS_STATUS,
                TASK_TABLE + "." + STATUS
        };
    }

    private StructureTaskDetails readTaskDetails(Cursor cursor) {
        StructureTaskDetails task = new StructureTaskDetails(cursor.getString(cursor.getColumnIndex(ID)));
        task.setTaskCode(cursor.getString(cursor.getColumnIndex(CODE)));
        task.setTaskEntity(cursor.getString(cursor.getColumnIndex(FOR)));
        task.setBusinessStatus(cursor.getString(cursor.getColumnIndex(BUSINESS_STATUS)));
        task.setTaskStatus(cursor.getString(cursor.getColumnIndex(STATUS)));
        return task;
    }
}
