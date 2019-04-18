package org.smartregister.reveal.interactor;

import android.location.Location;
import android.support.v4.util.Pair;

import com.google.common.annotations.VisibleForTesting;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.apache.commons.lang3.StringUtils;
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

import static org.smartregister.family.util.DBConstants.KEY.FIRST_NAME;
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
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_NAME;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY;

/**
 * Created by samuelgithengi on 3/18/19.
 */
public class TaskRegisterFragmentInteractor {

    private final StructureRepository structureRepository;
    private SQLiteDatabase database;
    private final Float locationBuffer;
    private final AppExecutors appExecutors;

    private TaskRegisterFragmentContract.Presenter presenter;

    public TaskRegisterFragmentInteractor(TaskRegisterFragmentContract.Presenter presenter) {
        this(presenter, RevealApplication.getInstance().getRepository().getReadableDatabase(),
                RevealApplication.getInstance().getAppExecutors(), Utils.getLocationBuffer());
    }

    @VisibleForTesting
    public TaskRegisterFragmentInteractor(TaskRegisterFragmentContract.Presenter presenter,
                                          SQLiteDatabase database, AppExecutors appExecutors,
                                          Float locationBuffer) {
        this.presenter = presenter;
        this.appExecutors = appExecutors;
        this.database = database;
        this.locationBuffer = locationBuffer;
        this.structureRepository = RevealApplication.getInstance().getStructureRepository();
    }

    private String mainSelect(String mainCondition) {
        String tableName = DatabaseKeys.TASK_TABLE;
        SmartRegisterQueryBuilder queryBuilder = new SmartRegisterQueryBuilder();
        queryBuilder.selectInitiateMainTable(tableName, mainColumns(tableName), ID);
        queryBuilder.customJoin(String.format(" LEFT JOIN %s ON %s.%s = %s.%s ",
                STRUCTURES_TABLE, tableName, FOR, STRUCTURES_TABLE, ID));
        queryBuilder.customJoin(String.format(" LEFT JOIN %s ON %s.%s = %s.%s ",
                SPRAYED_STRUCTURES, tableName, FOR, SPRAYED_STRUCTURES, DBConstants.KEY.BASE_ENTITY_ID));
        queryBuilder.customJoin(String.format(" LEFT JOIN %s ON %s.%s = %s.%s ",
                FAMILY, STRUCTURES_TABLE, ID, FAMILY, STRUCTURE_ID));
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
                SPRAYED_STRUCTURES + "." + STRUCTURE_NAME,
                SPRAYED_STRUCTURES + "." + FAMILY_NAME,
                SPRAYED_STRUCTURES + "." + SPRAY_STATUS,
                SPRAYED_STRUCTURES + "." + NOT_SRAYED_REASON,
                SPRAYED_STRUCTURES + "." + NOT_SRAYED_OTHER_REASON,
                STRUCTURES_TABLE + "." + ID + " AS " + STRUCTURE_ID,
                FAMILY + "." + FIRST_NAME
        };
    }


    public void findTasks(Pair<String, String[]> mainCondition, Location lastLocation, Location operationalAreaCenter, String houseLabel) {
        if (mainCondition == null || mainCondition.second == null || mainCondition.second.length != 2 || mainCondition.second[0] == null) {
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
                    TaskDetails taskDetails = readTaskDetails(cursor, lastLocation, operationalAreaCenter, houseLabel);
                    if (taskDetails.getDistanceFromUser() <= locationBuffer) {
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


    private TaskDetails readTaskDetails(Cursor cursor, Location lastLocation, Location operationalAreaCenter, String houseLabel) {
        TaskDetails task = new TaskDetails(cursor.getString(cursor.getColumnIndex(ID)));
        task.setTaskCode(cursor.getString(cursor.getColumnIndex(CODE)));
        task.setTaskEntity(cursor.getString(cursor.getColumnIndex(FOR)));
        task.setBusinessStatus(cursor.getString(cursor.getColumnIndex(BUSINESS_STATUS)));
        task.setTaskStatus(cursor.getString(cursor.getColumnIndex(STATUS)));
        Location location = new Location((String) null);
        location.setLatitude(cursor.getDouble(cursor.getColumnIndex(LATITUDE)));
        location.setLongitude(cursor.getDouble(cursor.getColumnIndex(LONGITUDE)));
        task.setLocation(location);
        if (Constants.Intervention.BCC.equals(task.getTaskCode())) {
            //set distance to -1 to always display on top of register
            task.setDistanceFromUser(-1);
        } else if (lastLocation != null) {
            task.setDistanceFromUser(location.distanceTo(lastLocation));
        } else {
            task.setDistanceFromUser(location.distanceTo(operationalAreaCenter));
            task.setDistanceFromCenter(true);
        }
        task.setStructureName(cursor.getString(cursor.getColumnIndex(NAME)));
        if (StringUtils.isBlank(task.getStructureName())) {
            task.setStructureName(cursor.getString(cursor.getColumnIndex(STRUCTURE_NAME)));
        }

        task.setFamilyName(cursor.getString(cursor.getColumnIndex(FIRST_NAME)));
        if (task.getFamilyName() == null) {
            task.setFamilyName(cursor.getString(cursor.getColumnIndex(FAMILY_NAME)));
        }

        if (task.getFamilyName() != null)
            task.setFamilyName(task.getFamilyName() + " " + houseLabel);

        task.setSprayStatus(cursor.getString(cursor.getColumnIndex(SPRAY_STATUS)));

        if (Constants.BusinessStatus.NOT_SPRAYED.equals(task.getBusinessStatus())) {
            String reason = cursor.getString(cursor.getColumnIndex(NOT_SRAYED_REASON));
            if (OTHER.equals(reason)) {
                reason = cursor.getString(cursor.getColumnIndex(NOT_SRAYED_OTHER_REASON));
            }
            task.setTaskDetails(reason);
        }
        task.setStructureId(cursor.getString(cursor.getColumnIndex(STRUCTURE_ID)));
        return task;
    }


    public void calculateDistanceFromUser(List<TaskDetails> tasks, Location location) {
        if (tasks == null)
            return;
        appExecutors.diskIO().execute(() -> {
            int structuresWithinBuffer = 0;
            for (TaskDetails taskDetails : tasks) {
                if (!Constants.Intervention.BCC.equals(taskDetails.getTaskCode())) {
                    taskDetails.setDistanceFromUser(taskDetails.getLocation().distanceTo(location));
                    taskDetails.setDistanceFromCenter(false);
                }
                if (taskDetails.getDistanceFromUser() <= locationBuffer) {
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
                    structureRepository.getLocationById(taskDetails.getStructureId());
            appExecutors.mainThread().execute(() -> {
                presenter.onStructureFound(structure, taskDetails);
            });
        });
    }

}
