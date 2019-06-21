package org.smartregister.reveal.interactor;

import android.support.annotation.VisibleForTesting;
import android.util.Log;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.apache.commons.lang3.ArrayUtils;
import org.smartregister.cursoradapter.SmartRegisterQueryBuilder;
import org.smartregister.family.util.Utils;
import org.smartregister.repository.StructureRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.Constants.Intervention;

import java.util.ArrayList;
import java.util.List;

import static org.smartregister.family.util.DBConstants.KEY.BASE_ENTITY_ID;
import static org.smartregister.family.util.DBConstants.KEY.DOB;
import static org.smartregister.family.util.DBConstants.KEY.FIRST_NAME;
import static org.smartregister.family.util.DBConstants.KEY.LAST_NAME;
import static org.smartregister.family.util.DBConstants.KEY.MIDDLE_NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.PLAN_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CODE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FOR;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURES_TABLE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_TABLE;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY_MEMBER;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class StructureTasksInteractor extends BaseInteractor implements StructureTasksContract.Interactor {


    private static final String TAG = StructureTasksInteractor.class.getName();
    private AppExecutors appExecutors;

    private SQLiteDatabase database;
    private StructureTasksContract.Presenter presenter;
    private StructureRepository structureRepository;

    public StructureTasksInteractor(StructureTasksContract.Presenter presenter) {
        this(presenter, RevealApplication.getInstance().getAppExecutors(), RevealApplication.getInstance().getRepository().getReadableDatabase(), RevealApplication.getInstance().getStructureRepository());
    }

    @VisibleForTesting
    protected StructureTasksInteractor(StructureTasksContract.Presenter presenter, AppExecutors appExecutors,
                                       SQLiteDatabase database, StructureRepository structureRepository) {
        super(presenter);
        this.presenter = presenter;
        this.appExecutors = appExecutors;
        this.database = database;
        this.structureRepository = structureRepository;
    }

    @Override
    public void findTasks(String structureId, String campaignId) {
        appExecutors.diskIO().execute(() -> {
            List<StructureTaskDetails> taskDetailsList = new ArrayList<>();
            Cursor cursor = null;
            try {
                cursor = database.rawQuery(getStructureSelect(String.format("%s=? AND %s=?", FOR, PLAN_ID)),
                        new String[]{structureId, campaignId});
                while (cursor.moveToNext()) {
                    taskDetailsList.add(readTaskDetails(cursor));
                }

                cursor.close();
                cursor = database.rawQuery(getMembersSelect(String.format("%s.%s=? AND %s=?",
                        STRUCTURES_TABLE, ID, PLAN_ID), getMemberColumns()), new String[]{structureId, campaignId});
                while (cursor.moveToNext()) {
                    taskDetailsList.add(readMemberTaskDetails(cursor));
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

    @Override
    public void getStructure(StructureTaskDetails details) {
        appExecutors.diskIO().execute(() -> {

            String structureId = details.getTaskEntity();
            if (Intervention.BLOOD_SCREENING.equals(details.getTaskCode()) ||
                    Intervention.CASE_CONFIRMATION.equals(details.getTaskCode())) {
                structureId = details.getStructureId();
            }
            org.smartregister.domain.Location structure =
                    structureRepository.getLocationById(structureId);
            appExecutors.mainThread().execute(() -> {
                presenter.onStructureFound(structure, details);
            });
        });
    }


    private String getStructureSelect(String mainCondition) {
        SmartRegisterQueryBuilder queryBuilder = new SmartRegisterQueryBuilder();
        queryBuilder.selectInitiateMainTable(TASK_TABLE, getStructureColumns(), ID);
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

    private String[] getMemberColumns() {
        String[] columns = getStructureColumns();
        String[] otherColumns = new String[]{
                "printf('%s %s %s'," + FIRST_NAME + "," + MIDDLE_NAME + "," + LAST_NAME + ") AS " + NAME,
                DOB,
                FAMILY_MEMBER + "." + STRUCTURE_ID
        };
        return ArrayUtils.addAll(columns, otherColumns);
    }

    private StructureTaskDetails readTaskDetails(Cursor cursor) {
        StructureTaskDetails task = new StructureTaskDetails(cursor.getString(cursor.getColumnIndex(ID)));
        task.setTaskCode(cursor.getString(cursor.getColumnIndex(CODE)));
        task.setTaskEntity(cursor.getString(cursor.getColumnIndex(FOR)));
        task.setBusinessStatus(cursor.getString(cursor.getColumnIndex(BUSINESS_STATUS)));
        task.setTaskStatus(cursor.getString(cursor.getColumnIndex(STATUS)));
        return task;
    }

    private StructureTaskDetails readMemberTaskDetails(Cursor cursor) {
        StructureTaskDetails task = readTaskDetails(cursor);
        String dob = cursor.getString(cursor.getColumnIndex(DOB));
        String dobString = Utils.getDuration(dob);
        dobString = dobString.contains("y") ? dobString.substring(0, dobString.indexOf("y")) : dobString;
        task.setTaskName(cursor.getString(cursor.getColumnIndex(NAME)) + ", " + dobString);
        task.setStructureId(cursor.getString(cursor.getColumnIndex(STRUCTURE_ID)));
        return task;
    }
}
