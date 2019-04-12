package org.smartregister.reveal.interactor;

import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.util.AppExecutors;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CODE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FOR;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NOT_SRAYED_OTHER_REASON;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.SPRAYED_STRUCTURES;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_TABLE;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class StructureTasksInteractor implements StructureTasksContract.Interactor {
    private AppExecutors appExecutors;

    private SQLiteDatabase database;

    public StructureTasksInteractor() {
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        database = RevealApplication.getInstance().getRepository().getReadableDatabase();
    }

    @Override
    public void findTasks(String structureId) {
        appExecutors.diskIO().execute(() -> {
        });
    }

    private String[] getColumns() {
        return new String[]{
                TASK_TABLE + "." + ID,
                TASK_TABLE + "." + CODE,
                TASK_TABLE + "." + FOR,
                TASK_TABLE + "." + BUSINESS_STATUS,
                TASK_TABLE + "." + STATUS,
                SPRAYED_STRUCTURES + "." + NOT_SRAYED_OTHER_REASON
        };
    }
}
