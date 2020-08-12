package org.smartregister.reveal.util;

import android.content.Context;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;
import net.sqlcipher.database.SQLiteException;

import org.smartregister.domain.Task;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.IndicatorDetails;
import org.smartregister.reveal.model.TaskDetails;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import timber.log.Timber;

/**
 * Created by ndegwamartin on 2019-09-27.
 */
public class IndicatorUtils {

    /**
     * Process task details from map of tasks (key : structureId)
     */
    public static List<TaskDetails> processTaskDetails(Map<String, Set<Task>> map) {

        List<TaskDetails> taskDetailsList = new ArrayList<>();

        for (Map.Entry<String, Set<Task>> entry : map.entrySet()) {

            for (Task task : entry.getValue()) {

                taskDetailsList.add(convertToTaskDetails(task));
            }

        }

        return taskDetailsList;

    }

    /**
     * Convert task to task details object
     *
     * @param task the task
     * @return TaskDetails object
     */

    public static TaskDetails convertToTaskDetails(Task task) {

        TaskDetails taskDetails = new TaskDetails(task.getIdentifier());

        taskDetails.setTaskCode(task.getCode());
        taskDetails.setTaskEntity(task.getForEntity());
        taskDetails.setBusinessStatus(task.getBusinessStatus());
        taskDetails.setTaskStatus(task.getStatus().name());
        taskDetails.setStructureId(task.getStructureId());

        return taskDetails;

    }

    public static IndicatorDetails processIndicators(List<TaskDetails> tasks) {

        Map<String, TaskDetails> indicatorDetailsMap = new HashMap<>();

        IndicatorDetails indicatorDetails = new IndicatorDetails();

        if (tasks != null) {

            for (int i = 0; i < tasks.size(); i++) {

                if (Constants.Intervention.IRS.equals(tasks.get(i).getTaskCode())) {

                    indicatorDetailsMap.put(tasks.get(i).getStructureId(), tasks.get(i));
                }

            }

            for (Map.Entry<String, TaskDetails> entry : indicatorDetailsMap.entrySet()) {

                if (Constants.BusinessStatusWrapper.SPRAYED.contains(entry.getValue().getBusinessStatus())) {

                    indicatorDetails.setSprayed(indicatorDetails.getSprayed() + 1);

                } else if (Constants.BusinessStatusWrapper.NOT_SPRAYED.contains(entry.getValue().getBusinessStatus())) {

                    indicatorDetails.setNotSprayed(indicatorDetails.getNotSprayed() + 1);

                } else if (Constants.BusinessStatusWrapper.NOT_VISITED.contains(entry.getValue().getBusinessStatus())) {

                    indicatorDetails.setNotVisited(indicatorDetails.getNotVisited() + 1);

                } else if (Constants.BusinessStatusWrapper.NOT_ELIGIBLE.contains(entry.getValue().getBusinessStatus())) {

                    indicatorDetails.setIneligible(indicatorDetails.getIneligible() + 1);

                }
            }

        }

        indicatorDetails.setTotalStructures(indicatorDetailsMap.size() - indicatorDetails.getIneligible());
        indicatorDetails.setProgress(indicatorDetails.getTotalStructures() > 0 ? Math.round(indicatorDetails.getSprayed() * 100 / indicatorDetails.getTotalStructures()) : 0);

        return indicatorDetails;
    }

    public static List<String> populateSprayIndicators(Context context, IndicatorDetails indicatorDetails) {

        int totalStructures = indicatorDetails.getTotalStructures();
        int sprayCoverage = indicatorDetails.getProgress();

        List<String> sprayIndicator = new ArrayList<>();

        sprayIndicator.add(context.getResources().getString(R.string.spray_coverage));
        sprayIndicator.add(context.getResources().getString(R.string.n_percent, sprayCoverage));

        int totalFound = (indicatorDetails.getSprayed() + indicatorDetails.getNotSprayed());

        sprayIndicator.add(context.getResources().getString(R.string.structures_remaining_90));
        sprayIndicator.add(String.valueOf(Math.round(totalStructures * 0.9) - indicatorDetails.getSprayed()));


        sprayIndicator.add(context.getResources().getString(R.string.total_structures));
        sprayIndicator.add(String.valueOf(totalStructures));


        sprayIndicator.add(context.getResources().getString(R.string.structures_not_visited));
        sprayIndicator.add(String.valueOf(indicatorDetails.getNotVisited()));


        sprayIndicator.add(context.getResources().getString(R.string.structures_visited_found));
        if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
            sprayIndicator.add(String.valueOf(totalFound));
        } else if (BuildConfig.BUILD_COUNTRY == Country.NAMIBIA) {
            sprayIndicator.add(String.valueOf(indicatorDetails.getFoundStructures()));

            sprayIndicator.add(context.getResources().getString(R.string.room_coverage));
            sprayIndicator.add(context.getResources().getString(R.string.n_percent, indicatorDetails.getRoomCoverage()));
        }


        sprayIndicator.add(context.getResources().getString(R.string.sprayed));
        sprayIndicator.add(String.valueOf(indicatorDetails.getSprayed()));

        sprayIndicator.add(context.getResources().getString(R.string.structures_not_sprayed));
        sprayIndicator.add(String.valueOf(indicatorDetails.getNotSprayed()));

        return sprayIndicator;
    }

    public static IndicatorDetails getNamibiaIndicators(String locationId, SQLiteDatabase sqLiteDatabase) {
        String query = "select count(s._id) as totStruct" +
                ", sum(case when ss.spray_status is null  then 1 else 0 end) as notVisitedStruct" +
                ", sum(case when ss.nSprayableTotal >0 then 1 else 0 end) as foundStruct" +
                ", sum(case when ss.ableToSprayFirst ='yes'  then 1 else 0 end) as sprayedStruct" +
                ", sum(case when ss.ableToSprayFirst ='no'  then 1 else 0 end) as notSprayedStruct" +
                ", round(sum(ifNull(nSprayedTotalFirst,0)+ifNull(nSprayedTotalMop,0))*100.0/sum(ss.nSprayableTotal)) as roomCov" +
                " from structure s" +
                " left join sprayed_structures ss on s._id=ss.id" +
                " where  parent_id=?";
        IndicatorDetails indicatorDetails = new IndicatorDetails();
        try (Cursor cursor = sqLiteDatabase.rawQuery(query, new String[]{locationId})) {
            if (cursor.moveToNext()) {
                indicatorDetails.setTotalStructures(cursor.getInt(cursor.getColumnIndex("totStruct")));
                indicatorDetails.setNotVisited(cursor.getInt(cursor.getColumnIndex("notVisitedStruct")));
                indicatorDetails.setFoundStructures(cursor.getInt(cursor.getColumnIndex("foundStruct")));
                indicatorDetails.setSprayed(cursor.getInt(cursor.getColumnIndex("sprayedStruct")));
                indicatorDetails.setNotSprayed(cursor.getInt(cursor.getColumnIndex("notSprayedStruct")));
                indicatorDetails.setRoomCoverage(cursor.getInt(cursor.getColumnIndex("roomCov")));
                indicatorDetails.setProgress((int) (indicatorDetails.getSprayed() * 100.0 / indicatorDetails.getTotalStructures()));
            }
        } catch (SQLiteException e) {
            Timber.e(e);
        }
        return indicatorDetails;
    }
}
