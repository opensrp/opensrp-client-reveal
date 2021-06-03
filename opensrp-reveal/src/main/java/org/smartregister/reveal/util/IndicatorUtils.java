package org.smartregister.reveal.util;

import android.content.Context;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;
import net.sqlcipher.database.SQLiteException;

import org.smartregister.domain.Event;
import org.smartregister.domain.Task;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.model.IndicatorDetails;
import org.smartregister.reveal.model.TaskDetails;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

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


    public static List<String> populateNamibiaSprayIndicators(Context context, IndicatorDetails indicatorDetails) {
        List<String> sprayIndicator = new ArrayList<>();
        sprayIndicator.add(context.getResources().getString(R.string.structures_targeted));
        sprayIndicator.add(String.valueOf(indicatorDetails.getTarget()));

        sprayIndicator.add(context.getResources().getString(R.string.structures_visited_found));
        sprayIndicator.add(String.valueOf(indicatorDetails.getFoundStructures()));

        sprayIndicator.add(context.getResources().getString(R.string.structures_sprayed));
        sprayIndicator.add(String.valueOf(indicatorDetails.getSprayed()));

        sprayIndicator.add(context.getResources().getString(R.string.structures_not_sprayed));
        sprayIndicator.add(String.valueOf(indicatorDetails.getNotSprayed()));


        return sprayIndicator;
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
        sprayIndicator.add(String.valueOf(totalFound));

        sprayIndicator.add(context.getResources().getString(R.string.sprayed));
        sprayIndicator.add(String.valueOf(indicatorDetails.getSprayed()));

        sprayIndicator.add(context.getResources().getString(R.string.structures_not_sprayed));
        sprayIndicator.add(String.valueOf(indicatorDetails.getNotSprayed()));

        return sprayIndicator;
    }

    public static IndicatorDetails getNamibiaIndicators(String locationId, String planId, SQLiteDatabase sqLiteDatabase) {
        String query = "select" +
                " sum(ifNull(ss.nSprayableTotal,0)) as foundStruct" +
                " ,sum(ifNull(ss.nSprayedTotalFirst,0)+ifNull(ss.nSprayedTotalMop,0)) as sprayedStruct" +
                " ,sum(ifNull(nSprayableTotal,0)- ifNull(nSprayedTotalFirst,0) - ifnull(nSprayedTotalMop,0)) as notSprayedStruct" +
                " from sprayed_structures ss " +
                " join structure s on s._id=ss.id" +
                " where parent_id=? and ss.plan_id=?";
        IndicatorDetails indicatorDetails = new IndicatorDetails();
        try (Cursor cursor = sqLiteDatabase.rawQuery(query, new String[]{locationId, planId})) {
            if (cursor.moveToNext()) {
                indicatorDetails.setFoundStructures(cursor.getInt(cursor.getColumnIndex("foundStruct")));
                indicatorDetails.setSprayed(cursor.getInt(cursor.getColumnIndex("sprayedStruct")));
                indicatorDetails.setNotSprayed(cursor.getInt(cursor.getColumnIndex("notSprayedStruct")));
            }
        } catch (SQLiteException e) {
            Timber.e(e);
        }
        return indicatorDetails;
    }

    public static IndicatorDetails processRwandaIndicators(List<TaskDetails> tasks){
        IndicatorDetails indicatorDetails = new IndicatorDetails();
       Set<String> taskIdentifiers = tasks.stream()
                                          .filter(taskDetails -> taskDetails.getTaskCode().equals("Cell Coordination") && (taskDetails.getBusinessStatus().equals("In Progress") || taskDetails.getBusinessStatus().equals("Complete")))
                                          .map(taskDetails -> taskDetails.getTaskId())
                                          .collect(Collectors.toSet());
        EventClientRepository eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
        List<Event> dataCaptured = eventClientRepository.getEventsByTaskIds(taskIdentifiers);
        Integer value  = dataCaptured.stream().map(event -> event.getObs()).map(obs -> obs.stream().filter(obsValue -> obsValue.getFieldCode().equals("health_education_above_16")).findFirst().get()).map(obs -> obs.getValue()).mapToInt(val -> Integer.parseInt(val.toString())).sum();
        indicatorDetails.setHealthEducatedChildren5To15(value);
        return indicatorDetails;
    }

    public static List<String> populateRwandaIndicators(Context context, IndicatorDetails indicatorDetails){
        List<String> indicators = new ArrayList<>();

        indicators.add("Health education ages 5-15 years");
        indicators.add(String.valueOf(indicatorDetails.getHealthEducatedChildren5To15()));

        indicators.add("Health education 16 years and above");
        indicators.add(String.valueOf(indicatorDetails.getHealthEducatedChildrenAbove16()));

        indicators.add("Vitamin A total 6-11 month");
        indicators.add(String.valueOf(indicatorDetails.getVitaminTreatedChildren6To11Months()));

        indicators.add("Vitamin A total 12-59 months");
        indicators.add(String.valueOf(indicatorDetails.getVitaminTreatedChildren12To59Months()));

        indicators.add("ALB/MEB total 12-59 months");
        indicators.add(String.valueOf(indicatorDetails.getAlbMebTreatedChildren12To59Months()));

        indicators.add("ALB/MEB total 5-15 years");
        indicators.add(String.valueOf(indicatorDetails.getAlbMebTreatedChildren5To15Years()));

        indicators.add("ALB/MEB total 16 years and above");
        indicators.add(String.valueOf(indicatorDetails.getAlbMebTreatedChildrenAbove16Years()));

        indicators.add("PZQ total 5-15 years");
        indicators.add(String.valueOf(indicatorDetails.getPzqTreatedChildren5To15Years()));

        indicators.add("PZQ total 16 years and above");
        indicators.add(String.valueOf(indicatorDetails.getPzqTreatedChildrenAbove16Years()));

        return  indicators;
    }
}
