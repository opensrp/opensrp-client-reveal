package org.smartregister.reveal.util;

import android.content.Context;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.domain.Task;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
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

                if (Constants.Intervention.IRS.equals(tasks.get(i).getTaskCode()) || Country.NIGERIA.equals(BuildConfig.BUILD_COUNTRY)) {

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

                } else if(Constants.BusinessStatusWrapper.DRUG_DISTRIBUTION_COMPLETE.contains(entry.getValue().getBusinessStatus())){

                    indicatorDetails.setCompleteDrugDistribution(indicatorDetails.getCompleteDrugDistribution() + 1);

                } else  if(Constants.BusinessStatusWrapper.DRUG_DISTRIBUTION_PARTIAL.contains(entry.getValue().getBusinessStatus())){

                    indicatorDetails.setPartialDrugDistribution(indicatorDetails.getPartialDrugDistribution() + 1);

                }
            }

        }

        indicatorDetails.setTotalStructures(indicatorDetailsMap.size() - indicatorDetails.getIneligible());

        indicatorDetails.setProgress(indicatorDetails.getTotalStructures() > 0 ? Math.round(indicatorDetails.getSprayed() * 100 / indicatorDetails.getTotalStructures()) : 0);

        return indicatorDetails;
    }



    public static List<String> populateNamibiaSprayIndicators(Context context, IndicatorDetails indicatorDetails) {
        List<String> sprayIndicator = new ArrayList<>();
        sprayIndicator.add(context.getResources().getString(R.string.total_structures_targeted));
        sprayIndicator.add(String.valueOf(indicatorDetails.getTarget()));

        sprayIndicator.add(context.getResources().getString(R.string.structures_visited_found));
        sprayIndicator.add(String.valueOf(indicatorDetails.getFoundStructures()));

        sprayIndicator.add(context.getResources().getString(R.string.structures_sprayed));
        sprayIndicator.add(String.valueOf(indicatorDetails.getSprayed()));


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

    public static List<String> populateNigeriaIndicators(Context context,IndicatorDetails indicatorDetails){
        List<String> indicators = new ArrayList<>();

        indicators.add(context.getResources().getString(R.string.structure_total));
        indicators.add(String.valueOf(indicatorDetails.getTotalStructures()));


        int structureVisited = indicatorDetails.getTotalStructures() - indicatorDetails.getIneligible() - indicatorDetails.getNotVisited();
        indicators.add(context.getResources().getString(R.string.structure_visited));
        indicators.add(String.valueOf(structureVisited));

        indicators.add(context.getResources().getString(R.string.structure_not_visited));
        indicators.add(String.valueOf(indicatorDetails.getNotVisited()));

        indicators.add(context.getResources().getString(R.string.structure_confirmed_eligible));
        indicators.add(String.valueOf(indicatorDetails.getFoundStructures()));


        indicators.add(context.getResources().getString(R.string.structure_complete_drug_distribution));
        indicators.add(String.valueOf(indicatorDetails.getCompleteDrugDistribution()));


        indicators.add(context.getResources().getString(R.string.structure_partial_drug_distribution));
        indicators.add(String.valueOf(indicatorDetails.getPartialDrugDistribution()));

        indicators.add(context.getResources().getString(R.string.individual_total_number_of_children_eligible_3_to_49_mos));
        indicators.add(String.valueOf(indicatorDetails.getChildrenEligible()));

        indicators.add(context.getResources().getString(R.string.individual_total_treated_3_to_59_mos));
        indicators.add(String.valueOf(indicatorDetails.getTotalIndividualTreated()));

        return indicators;
    }

    public static IndicatorDetails processIndicatorsNigeria(List<TaskDetails> tasks) {
        //TODO: move to other method, and perform logic based on interventions and country
        Map<String, List<TaskDetails>> indicatorDetailsMap = new HashMap<>();
        IndicatorDetails indicatorDetails = new IndicatorDetails();
        List<String> structures = new ArrayList<>();
        if (tasks != null) {

            for (int i = 0; i < tasks.size(); i++) {

                if (Constants.Intervention.IRS.equals(tasks.get(i).getTaskCode()) || Country.NIGERIA.equals(BuildConfig.BUILD_COUNTRY)) {
                    String structureId = tasks.get(i).getStructureId();
                    List<TaskDetails> taskDetails = indicatorDetailsMap.get(structureId);
                    if(taskDetails == null){
                        taskDetails  = new ArrayList<>();
                    }
                    taskDetails.add(tasks.get(i));
                    indicatorDetailsMap.put(tasks.get(i).getStructureId(),taskDetails);
                }

            }

            for(Map.Entry<String,List<TaskDetails>> entry : indicatorDetailsMap.entrySet()){

                    structures.add(entry.getKey());
                for(TaskDetails task: entry.getValue()){
                        if((task.getTaskCode().equals(Constants.Intervention.MDA_DISPENSE) || task.getTaskCode().equals(Constants.Intervention.REGISTER_FAMILY)) && Constants.BusinessStatusWrapper.NOT_ELIGIBLE.contains(task.getBusinessStatus())){
                            indicatorDetails.setIneligible(indicatorDetails.getIneligible() + 1);
                            break;
                        }
                        if(Constants.BusinessStatusWrapper.NOT_VISITED.equals(task.getBusinessStatus())){
                            indicatorDetails.setNotVisited(indicatorDetails.getNotVisited() + 1);
                            break;
                        }

                        if(task.getTaskCode().equals(Constants.Intervention.MDA_DISPENSE) && Constants.BusinessStatusWrapper.DRUG_DISTRIBUTION_COMPLETE.contains(task.getBusinessStatus())){
                            indicatorDetails.setCompleteDrugDistribution(indicatorDetails.getCompleteDrugDistribution() + 1);
                            break;
                        }

                        if(task.getTaskCode().equals(Constants.Intervention.MDA_DISPENSE) && Constants.BusinessStatusWrapper.DRUG_DISTRIBUTION_PARTIAL.contains(task.getBusinessStatus())){
                            indicatorDetails.setPartialDrugDistribution(indicatorDetails.getPartialDrugDistribution() + 1);
                        }
                        if(task.getTaskCode().equals(Constants.Intervention.REGISTER_FAMILY)  && Constants.BusinessStatusWrapper.SPRAYED.contains(task.getBusinessStatus())) {
                            indicatorDetails.setFoundStructures(indicatorDetails.getFoundStructures() + 1);
                        }
                }

            }

            //TODO: the following should be encoded in tasks. or we must have enough data on local db to read these.
            indicatorDetails.setTotalIndividualTreated(getChildren(true, structures));
            indicatorDetails.setChildrenEligible(getChildren(false, structures));

        }

        indicatorDetails.setTotalStructures(indicatorDetailsMap.size());
        return indicatorDetails;
    }

    private static int getChildren(boolean treated,List<String> structureIds){
        SQLiteDatabase database = RevealApplication.getInstance().getRepository().getReadableDatabase();
        int totalIndividualsTreated = 0;
        StringBuilder structures = new StringBuilder("");
        for(String structureId : structureIds){
            if(structureIds.size() == structureIds.indexOf(structureId) + 1 ){
                structures.append(String.format("'%s'",structureId));
            } else {
                structures.append(String.format("'%s',",structureId));
            }
        }
        String query;
        if(treated){
            query = String.format("SELECT COUNT(*) FROM %s WHERE %s = 'Yes' AND %s = 'twelveToFiftyNine' and %s in (%s)", FamilyConstants.TABLE_NAME.FAMILY_MEMBER, Constants.DatabaseKeys.ADMINISTERED_SPAQ,Constants.DatabaseKeys.JOB_AID,Constants.DatabaseKeys.STRUCTURE_ID,structures.toString());
        } else{
            query  = String.format("SELECT COUNT(*) FROM  %s where %s IS NOT NULL", FamilyConstants.TABLE_NAME.FAMILY_MEMBER, Constants.DatabaseKeys.JOB_AID,structures.toString());
        }

        Cursor cursor = null;
        try{
            cursor = database.rawQuery(query,new String[]{});
            while (cursor.moveToNext()) {
                totalIndividualsTreated = cursor.getInt(0);
            }
        } catch (Exception e) {
            Timber.e(e, "Error reading %s  table",FamilyConstants.TABLE_NAME.FAMILY_MEMBER);
        }
        return totalIndividualsTreated;
    }
}



