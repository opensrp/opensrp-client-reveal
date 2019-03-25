package org.smartregister.reveal.interactor;

import android.location.Location;
import android.support.v4.util.Pair;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.cursoradapter.SmartRegisterQueryBuilder;
import org.smartregister.family.util.DBConstants;
import org.smartregister.repository.StructureRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.TaskRegisterFragmentContract;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.util.Utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CODE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FAMILY_NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FOR;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LATITUDE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LONGITUDE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NOT_SRAYED_OTHER_REASON;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NOT_SRAYED_REASON;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.OTHER;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.SPRAYED_STRUCTURES;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.SPRAY_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURES_TABLE;

/**
 * Created by samuelgithengi on 3/18/19.
 */
public class TaskRegisterFragmentInteractor {

    private SQLiteDatabase database;
    private AppExecutors appExecutors;

    private TaskRegisterFragmentContract.Presenter presenter;

    public TaskRegisterFragmentInteractor(AppExecutors appExecutors, TaskRegisterFragmentContract.Presenter presenter) {
        this.appExecutors = appExecutors;
        this.presenter = presenter;
        database = RevealApplication.getInstance().getRepository().getReadableDatabase();
    }

    private String mainSelect(String mainCondition) {
        String tableName = DatabaseKeys.TASK_TABLE;
        SmartRegisterQueryBuilder queryBuilder = new SmartRegisterQueryBuilder();
        queryBuilder.SelectInitiateMainTable(tableName, mainColumns(tableName), ID);
        queryBuilder.customJoin(String.format(" JOIN %s ON %s.%s = %s.%s  COLLATE NOCASE",
                STRUCTURES_TABLE, tableName, FOR, STRUCTURES_TABLE, ID));
        queryBuilder.customJoin(String.format(" LEFT JOIN %s ON %s.%s = %s.%s  COLLATE NOCASE",
                SPRAYED_STRUCTURES, tableName, FOR, SPRAYED_STRUCTURES, DBConstants.KEY.BASE_ENTITY_ID));
        return queryBuilder.mainCondition(mainCondition);
    }

    private String[] mainColumns(String tableName) {
        return new String[]{
                tableName + "." + ID,
                tableName + "." + CODE,
                tableName + "." + FOR,
                tableName + "." + BUSINESS_STATUS,
                tableName + "." + STATUS,
                STRUCTURES_TABLE + "." + LATITUDE,
                STRUCTURES_TABLE + "." + LONGITUDE,
                STRUCTURES_TABLE + "." + NAME,
                SPRAYED_STRUCTURES + "." + FAMILY_NAME,
                SPRAYED_STRUCTURES + "." + SPRAY_STATUS,
                SPRAYED_STRUCTURES + "." + NOT_SRAYED_REASON,
                SPRAYED_STRUCTURES + "." + NOT_SRAYED_OTHER_REASON
        };
    }


    public void findTasks(Pair<String, String[]> mainCondition, Location lastLocation) {
        if (mainCondition.second == null || mainCondition.second[0] == null) {
            presenter.onTasksFound(null, 0);
            return;
        }
        appExecutors.diskIO().execute(() -> {
            List<TaskDetails> tasks = new ArrayList<>();
            int structuresWithinBuffer = 0;
            String query = mainSelect(mainCondition.first);
            Cursor cursor = null;
            try {
                cursor = database.rawQuery(query, mainCondition.second);
                while (cursor.moveToNext()) {
                    TaskDetails taskDetails = readTaskDetails(cursor, lastLocation);
                    if (taskDetails.getDistanceFromUser() <= Utils.getLocationBuffer()) {
                        structuresWithinBuffer += 1;
                    }
                    tasks.add(taskDetails);
                }
            } finally {
                if (cursor != null) {
                    cursor.close();
                }
            }
            int finalStructuresWithinBuffer = structuresWithinBuffer;
            appExecutors.mainThread().execute(() -> {
                Collections.sort(tasks);
                presenter.onTasksFound(tasks, finalStructuresWithinBuffer);
            });
        });

    }


    private TaskDetails readTaskDetails(Cursor cursor, Location lastLocation) {
        TaskDetails task = new TaskDetails(cursor.getString(cursor.getColumnIndex(ID)));
        task.setTaskCode(cursor.getString(cursor.getColumnIndex(CODE)));
        task.setTaskEntity(cursor.getString(cursor.getColumnIndex(FOR)));
        task.setBusinessStatus(cursor.getString(cursor.getColumnIndex(BUSINESS_STATUS)));
        task.setTaskStatus(cursor.getString(cursor.getColumnIndex(STATUS)));
        Location location = new Location((String) null);
        location.setLatitude(cursor.getDouble(cursor.getColumnIndex(LATITUDE)));
        location.setLongitude(cursor.getDouble(cursor.getColumnIndex(LONGITUDE)));
        task.setLocation(location);
        if (lastLocation != null) {
            task.setDistanceFromUser(location.distanceTo(lastLocation));
        }
        task.setStructureName(cursor.getString(cursor.getColumnIndex(NAME)));
        task.setFamilyName(cursor.getString(cursor.getColumnIndex(FAMILY_NAME)));
        task.setSprayStatus(cursor.getString(cursor.getColumnIndex(SPRAY_STATUS)));

        if (Constants.BusinessStatus.NOT_SPRAYED.equals(task.getBusinessStatus())) {
            String reason = cursor.getString(cursor.getColumnIndex(NOT_SRAYED_REASON));
            if (OTHER.equals(reason)) {
                reason = cursor.getString(cursor.getColumnIndex(NOT_SRAYED_OTHER_REASON));
            }
            task.setTaskDetails(reason);
        }
        return task;
    }


    public void calculateDistanceFromUser(List<TaskDetails> tasks, Location location) {
        if (tasks == null)
            return;
        appExecutors.diskIO().execute(() -> {
            int structuresWithinBuffer = 0;
            for (TaskDetails taskDetails : tasks) {
                taskDetails.setDistanceFromUser(taskDetails.getLocation().distanceTo(location));
                if (taskDetails.getDistanceFromUser() <= Utils.getLocationBuffer()) {
                    structuresWithinBuffer += 1;
                }
            }
            Collections.sort(tasks);
            int finalStructuresWithinBuffer = structuresWithinBuffer;
            appExecutors.mainThread().execute(() -> {
                presenter.onTasksFound(tasks, finalStructuresWithinBuffer);
            });
        });

    }


    public void getStructure(TaskDetails taskDetails) {
        appExecutors.diskIO().execute(() -> {
            org.smartregister.domain.Location structure =
                    RevealApplication.getInstance().getStructureRepository().getLocationById(taskDetails.getTaskEntity());
            appExecutors.mainThread().execute(() -> {
                presenter.onStructureFound(structure, taskDetails);
            });
        });
    }

}
