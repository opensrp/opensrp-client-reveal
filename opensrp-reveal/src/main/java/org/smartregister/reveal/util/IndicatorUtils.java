package org.smartregister.reveal.util;

import android.content.Context;
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
        Map<String, List<TaskDetails>> indicatorDetailsMap = new HashMap<>();
        IndicatorDetails indicatorDetails = new IndicatorDetails();
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
                for(TaskDetails task: entry.getValue()){
                    if(task.getTaskCode().equals(Constants.Intervention.REGISTER_FAMILY.equals(task.getTaskCode()) && Constants.BusinessStatus.NOT_VISITED.equals(task.getBusinessStatus()))){
                        indicatorDetails.setNotVisited(indicatorDetails.getNotVisited() + 1);
                    }
                    if(Constants.BusinessStatusWrapper.NOT_ELIGIBLE.contains(task.getBusinessStatus())){
                        indicatorDetails.setIneligible(indicatorDetails.getIneligible() + 1);
                        break;
                    }
                    if(task.getTaskCode().equals(Constants.Intervention.MDA_DISPENSE)  && Constants.BusinessStatusWrapper.MDA_DISPENSE_ELIGIBLE_STATUS.contains(task.getBusinessStatus())) {
                        indicatorDetails.setFoundStructures(indicatorDetails.getFoundStructures() + 1);
                        break;
                    }

                }
                indicatorDetails.setCompleteDrugDistribution(indicatorDetails.getCompleteDrugDistribution() + calculateDrugCompletion(entry.getValue()));
                indicatorDetails.setPartialDrugDistribution(indicatorDetails.getPartialDrugDistribution() + calculatePartialDrugDistribution(entry.getValue()));
                indicatorDetails.setChildrenEligible(indicatorDetails.getChildrenEligible() + calculateChildrenEligible(entry.getValue()));
                indicatorDetails.setTotalIndividualTreated(indicatorDetails.getTotalIndividualTreated() + calculateChildrenTreated(entry.getValue()));

            }

        }

        indicatorDetails.setTotalStructures(indicatorDetailsMap.size() - indicatorDetails.getIneligible());
        return indicatorDetails;
    }


    private static int calculateDrugCompletion(List<TaskDetails> tasks ) {
        boolean dispenseComplete = false;
        boolean adherenceComplete = false;
        boolean  reconComplete = false;

        for(TaskDetails task : tasks){
            if(Constants.Intervention.MDA_DISPENSE.equals(task.getTaskCode()) && Constants.BusinessStatus.SMC_COMPLETE.equals(task.getBusinessStatus())){
                dispenseComplete = true;
            }else if(Constants.Intervention.MDA_DRUG_RECON.equals(task.getTaskCode()) && Constants.BusinessStatus.COMPLETE.equals(task.getBusinessStatus())) {
                reconComplete = true;
            }else if(Constants.Intervention.MDA_ADHERENCE.equals(task.getTaskCode()) && Constants.BusinessStatus.SPAQ_COMPLETE.equals(task.getBusinessStatus())){
                adherenceComplete = true;
            }
        }
        return (dispenseComplete  && reconComplete && adherenceComplete) ? 1 : 0;
    }

    private  static int calculatePartialDrugDistribution(List<TaskDetails> tasks){
        boolean dispenseComplete = false;
        boolean adherenceComplete = false;
        boolean reconComplete = false;
        for(TaskDetails task : tasks){
            if(Constants.Intervention.MDA_DISPENSE.equals(task.getTaskCode()) && Constants.BusinessStatus.SMC_COMPLETE.equals(task.getBusinessStatus())){
                dispenseComplete = true;
            }else if(Constants.Intervention.MDA_DRUG_RECON.equals(task.getTaskCode()) && Constants.BusinessStatus.COMPLETE.equals(task.getBusinessStatus())) {
                reconComplete = true;
            }else if(Constants.Intervention.MDA_ADHERENCE.equals(task.getTaskCode()) && Constants.BusinessStatus.SPAQ_COMPLETE.equals(task.getBusinessStatus())){
                adherenceComplete = true;
            }
        }
        return (dispenseComplete && (!adherenceComplete || !reconComplete)) ? 1 : 0;
    }

    private static int calculateChildrenEligible(List<TaskDetails> tasks){
        int childrenEligible = 0;
        for(TaskDetails task: tasks){
            if(task.getTaskCode().equals(Constants.Intervention.MDA_DISPENSE)  && Constants.BusinessStatusWrapper.MDA_DISPENSE_ELIGIBLE_STATUS.contains(task.getBusinessStatus())){
                childrenEligible++;
            }
        }
        return childrenEligible;
    }

    private static int calculateChildrenTreated(List<TaskDetails> tasks) {
        int treated = 0;
        boolean reconComplete = false;

        for(TaskDetails task : tasks){
            if(Constants.Intervention.MDA_DRUG_RECON.equals(task.getTaskCode()) && Constants.BusinessStatus.COMPLETE.equals(task.getBusinessStatus())) {
                reconComplete = true;
            }
            if(Constants.Intervention.MDA_DISPENSE.equals(task.getTaskCode()) && Constants.BusinessStatus.SMC_COMPLETE.equals(task.getBusinessStatus())){
                treated++;
            }
        }
        return reconComplete ? treated : 0;
    }
}



