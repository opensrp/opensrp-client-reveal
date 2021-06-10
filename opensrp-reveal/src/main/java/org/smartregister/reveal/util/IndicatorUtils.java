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

import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.IN_PROGRESS;
import static org.smartregister.reveal.util.Constants.Intervention.CELL_COORDINATION;
import static org.smartregister.reveal.util.Constants.JsonForm.HEALTH_EDUCATION_5_TO_15;
import static org.smartregister.reveal.util.Constants.JsonForm.HEALTH_EDUCATION_ABOVE_16;
import static org.smartregister.reveal.util.Constants.JsonForm.SUM_TREATED_1_TO_4;
import static org.smartregister.reveal.util.Constants.JsonForm.SUM_TREATED_5_TO_15;
import static org.smartregister.reveal.util.Constants.JsonForm.SUM_TREATED_6_TO_11_MOS;
import static org.smartregister.reveal.util.Constants.JsonForm.SUM_TREATED_ABOVE_16;

/**
 * Created by ndegwamartin on 2019-09-27.
 */
public class IndicatorUtils {


    public static final String VITAMIN_A = "Vitamin A";
    public static final String ALB_MEB = "ALB/MEB";
    public static final String PZQ = "PZQ";
    public static final String NTD_TREATED = "ntd_treated";

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
        Integer value = 0;
       Set<String> taskIdentifiers = tasks.stream()
                                          .filter(taskDetails -> taskDetails.getTaskCode().equals(CELL_COORDINATION) && (taskDetails.getBusinessStatus().equals(IN_PROGRESS) || taskDetails.getBusinessStatus().equals(COMPLETE)))
                                          .map(taskDetails -> taskDetails.getTaskId())
                                          .collect(Collectors.toSet());
        EventClientRepository eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();

        List<Event> dataCaptured = eventClientRepository.getEventsByTaskIds(taskIdentifiers);
        value  = dataCaptured.stream().map(event -> event.getObs())
                                              .map(obs -> obs.stream().filter(obsValue -> obsValue.getFieldCode().equals(HEALTH_EDUCATION_5_TO_15)).findFirst().get())
                                              .map(obs -> obs.getValue()).mapToInt(val -> Integer.parseInt(val.toString())).sum();
        indicatorDetails.setHealthEducatedChildren5To15(value);

        value = dataCaptured.stream().map(event -> event.getObs())
                .map(obs -> obs.stream().filter(obsValue -> obsValue.getFieldCode().equals(HEALTH_EDUCATION_ABOVE_16)).findFirst().get())
                .map(obs -> obs.getValue()).mapToInt(val -> Integer.parseInt(val.toString())).sum();
        indicatorDetails.setHealthEducatedChildrenAbove16(value);

        value = dataCaptured.stream().map(event -> event.getObs())
                .filter(obs -> obs.stream().filter(val -> val.getFieldCode().equals(NTD_TREATED) && val.getValue().equals(VITAMIN_A)).findAny().isPresent())
                .map(obs -> obs.stream().filter(obsValue -> obsValue.getFieldCode().equals(SUM_TREATED_6_TO_11_MOS)).findFirst().get())
                .map(obs -> obs.getValue()).mapToInt(val -> Integer.parseInt(val.toString())).sum();
        indicatorDetails.setVitaminTreatedChildren6To11Months(value);

        value = dataCaptured.stream().map(event -> event.getObs())
                .filter(obs -> obs.stream().filter(val -> val.getFieldCode().equals(NTD_TREATED) && val.getValue().equals(VITAMIN_A)).findAny().isPresent())
                .map(obs -> obs.stream().filter(obsValue -> obsValue.getFieldCode().equals(SUM_TREATED_1_TO_4)).findFirst().get())
                .map(obs -> obs.getValue()).mapToInt(val -> Integer.parseInt(val.toString())).sum();
        indicatorDetails.setVitaminTreatedChildren12To59Months(value);

        value = dataCaptured.stream().map(event -> event.getObs())
                .filter(obs -> obs.stream().filter(val -> val.getFieldCode().equals(NTD_TREATED) && val.getValue().equals(ALB_MEB)).findAny().isPresent())
                .map(obs -> obs.stream().filter(obsValue -> (obsValue.getFieldCode().equals(SUM_TREATED_1_TO_4))).findFirst().get())
                .map(obs -> obs.getValue()).mapToInt(val -> Integer.parseInt(val.toString())).sum();
        indicatorDetails.setAlbMebTreatedChildren12To59Months(value);

        value = dataCaptured.stream().map(event -> event.getObs())
                .filter(obs -> obs.stream().filter(val -> val.getFieldCode().equals(NTD_TREATED) && val.getValue().equals(ALB_MEB)).findAny().isPresent())
                .map(obs -> obs.stream().filter(obsValue -> obsValue.getFieldCode().equals(SUM_TREATED_5_TO_15)).findFirst().get())
                .map(obs -> obs.getValue()).mapToInt(val -> Integer.parseInt(val.toString())).sum();
        indicatorDetails.setAlbMebTreatedChildren5To15Years(value);

        value = dataCaptured.stream().map(event -> event.getObs())
                .filter(obs -> obs.stream().filter(val -> val.getFieldCode().equals(NTD_TREATED) && val.getValue().equals(PZQ)).findAny().isPresent())
                .map(obs -> obs.stream().filter(obsValue -> obsValue.getFieldCode().equals(SUM_TREATED_5_TO_15)).findFirst().get())
                .map(obs -> obs.getValue()).mapToInt(val -> Integer.parseInt(val.toString())).sum();
        indicatorDetails.setPzqTreatedChildren5To15Years(value);

        value = dataCaptured.stream().map(event -> event.getObs())
                .filter(obs -> obs.stream().filter(val -> val.getValue().equals(ALB_MEB)).findAny().isPresent())
                .map(obs -> obs.stream().filter(obsValue -> obsValue.getFieldCode().equals(SUM_TREATED_ABOVE_16)).findFirst().get())
                .map(obs -> obs.getValue()).mapToInt(val -> Integer.parseInt(val.toString())).sum();
        indicatorDetails.setAlbMebTreatedChildrenAbove16Years(value);

        value = dataCaptured.stream().map(event -> event.getObs())
                .filter(obs -> obs.stream().filter(val -> val.getValue().equals(PZQ)).findAny().isPresent())
                .map(obs -> obs.stream().filter(obsValue -> obsValue.getFieldCode().equals(SUM_TREATED_ABOVE_16)).findFirst().get())
                .map(obs -> obs.getValue()).mapToInt(val -> Integer.parseInt(val.toString())).sum();
        indicatorDetails.setPzqTreatedChildrenAbove16Years(value);


        return indicatorDetails;
    }

    public static List<String> populateRwandaIndicators(Context context, IndicatorDetails indicatorDetails){
        List<String> indicators = new ArrayList<>();

        indicators.add(context.getResources().getString(R.string.health_education_ages_5_to_15_years));
        indicators.add(String.valueOf(indicatorDetails.getHealthEducatedChildren5To15()));

        indicators.add(context.getResources().getString(R.string.health_education_16_years_and_above));
        indicators.add(String.valueOf(indicatorDetails.getHealthEducatedChildrenAbove16()));

        indicators.add(context.getResources().getString(R.string.vitamina_total_6_to_11_months));
        indicators.add(String.valueOf(indicatorDetails.getVitaminTreatedChildren6To11Months()));

        indicators.add(context.getResources().getString(R.string.vitamina_total_12_to_59_months));
        indicators.add(String.valueOf(indicatorDetails.getVitaminTreatedChildren12To59Months()));

        indicators.add(context.getResources().getString(R.string.alb_meb_total_12_to_59_months));
        indicators.add(String.valueOf(indicatorDetails.getAlbMebTreatedChildren12To59Months()));

        indicators.add(context.getResources().getString(R.string.alb_meb_total_5_to_15_years));
        indicators.add(String.valueOf(indicatorDetails.getAlbMebTreatedChildren5To15Years()));

        indicators.add(context.getResources().getString(R.string.alb_meb_total_16_years_and_above));
        indicators.add(String.valueOf(indicatorDetails.getAlbMebTreatedChildrenAbove16Years()));

        indicators.add(context.getResources().getString(R.string.pzq_total_5_to_15_years));
        indicators.add(String.valueOf(indicatorDetails.getPzqTreatedChildren5To15Years()));

        indicators.add(context.getResources().getString(R.string.pzq_total_16_years_and_above));
        indicators.add(String.valueOf(indicatorDetails.getPzqTreatedChildrenAbove16Years()));

        return  indicators;
    }
}
