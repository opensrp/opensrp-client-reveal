package org.smartregister.reveal.dao;

import android.location.Location;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.dao.AbstractDao;
import org.smartregister.domain.Task;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.smartregister.reveal.util.FamilyJsonFormUtils.getAge;

public class TaskDetailsDao extends AbstractDao {

    public static TaskDetailsDao getInstance() {
        return new TaskDetailsDao();
    }

    public Map<String, String> fetchFamilyMemberDetails(String operationalAreaId) {
        Map<String, String> results = new HashMap<>();

        String sql = "Select structure._id as _id , " +
                "group_concat(ec_family_member.first_name||' '||ec_family_member.last_name||' '||ec_family_member.phone_number) family_details " +
                "FROM structure " +
                "LEFT JOIN ec_family ON structure._id = ec_family.structure_id AND ec_family.date_removed IS NULL collate nocase  " +
                "LEFT JOIN ec_family_member ON ec_family.base_entity_id = ec_family_member.relational_id AND ec_family_member.date_removed IS NULL collate nocase  " +
                "LEFT JOIN sprayed_structures ON structure._id = sprayed_structures.base_entity_id collate nocase  " +
                (StringUtils.isBlank(operationalAreaId) ? "" : "WHERE parent_id='" + operationalAreaId + "'  ") +
                "GROUP BY structure._id";

        DataMap<Void> voidDataMap = cursor -> {
            results.put(getCursorValue(cursor, "_id"), getCursorValue(cursor, "family_details"));
            return null;
        };
        readData(sql, voidDataMap);

        return results;
    }

    public List<TaskDetails> fetchStructures(String operationalAreaId, String planId, Location lastLocation, Location operationalAreaCenter) {
        String sqlPhones = "select relational_id , group_concat(phone_number) phone_numbers from ec_family_member group by relational_id";

        Map<String, String> numbers = new HashMap<>();

        DataMap<Void> voidDataMap = cursor -> {
            numbers.put(getCursorValue(cursor, "relational_id"), getCursorValue(cursor, "phone_numbers"));
            return null;
        };

        readData(sqlPhones, voidDataMap);

        Map<String, String> memberDetails = fetchFamilyMemberDetails(operationalAreaId);

        String sql = "";
        if (StringUtils.isBlank(planId) || StringUtils.isBlank(operationalAreaId)) {
            sql =
                    "select ec_family.base_entity_id , ec_family.structure_id , ec_family.first_name fam_name , structure.latitude , structure.longitude, ifnull(task.business_status,'Not Visited') business_status , task.status task_status , '" + Constants.Intervention.REGISTER_FAMILY + "' code " +
                            "from ec_family " +
                            "inner join structure on ec_family.structure_id = structure._id " +
                            "left join task on task.structure_id = ec_family.structure_id and task.code = '" + Constants.Intervention.STRUCTURE_VISITED + "' and ec_family.structure_id is not null " +
                            "union " +
                            "select ec_family.base_entity_id , ec_family.structure_id , ec_family.first_name , null latitude , null longitude, ifnull(task.business_status,'Not Visited') business_status , task.status , '" + Constants.InterventionType.FLOATING_FAMILY + "'code " +
                            "from ec_family " +
                            "left join task on task.for = ec_family.base_entity_id and task.code = '" + Constants.Intervention.FLOATING_FAMILY_REGISTRATION + "' and ec_family.structure_id is null ";
        } else {
            sql =
                    "select ec_family.base_entity_id , ec_family.structure_id , ec_family.first_name fam_name , structure.latitude , structure.longitude, ifnull(task.business_status,'Not Visited') business_status , task.status task_status , '" + Constants.Intervention.REGISTER_FAMILY + "' code " +
                            "from ec_family " +
                            "inner join structure on ec_family.structure_id = structure._id " +
                            "inner join task on task.structure_id = ec_family.structure_id and task.code = '" + Constants.Intervention.STRUCTURE_VISITED + "'  " +
                            "and task.plan_id = '" + planId + "' and  task.group_id = '" + operationalAreaId + "' " +
                            "union  " +
                            "select null base_entity_id , structure._id structure_id , '' name , structure.latitude , structure.longitude, ifnull(task.business_status,'Not Visited') , task.status , '" + Constants.Intervention.REGISTER_FAMILY + "' code " +
                            "from structure " +
                            "left join task on task.structure_id = structure._id and task.code = '" + Constants.Intervention.STRUCTURE_VISITED + "'  " +
                            "where structure.parent_id = '" + operationalAreaId + "' " +
                            "and structure._id not in ( " +
                            "select structure_id from task where code = '" + Constants.Intervention.STRUCTURE_VISITED + "' and  " +
                            "task.plan_id = '" + planId + "' and  task.group_id = '" + operationalAreaId + "' and structure_id is not null ) " +
                            "union " +
                            "select ec_family.base_entity_id , ec_family.structure_id , ec_family.first_name , null latitude , null longitude, ifnull(task.business_status,'Not Visited') business_status , task.status , '" + Constants.InterventionType.FLOATING_FAMILY + "'code " +
                            "from ec_family " +
                            "inner join task on task.for = ec_family.base_entity_id and task.code = '" + Constants.Intervention.FLOATING_FAMILY_REGISTRATION + "'  " +
                            "and task.plan_id = '" + planId + "' and  task.group_id = '" + operationalAreaId + "' ";
        }


        DataMap<TaskDetails> dataMap = cursor -> {
            TaskDetails taskDetails = new TaskDetails(getCursorValue(cursor, "structure_id", ""));

            String familyName = getCursorValue(cursor, "fam_name");
            if (StringUtils.isNotBlank(familyName))
                taskDetails.setStructureName(familyName + " Family");
            taskDetails.setFamilyBaseEntityID(getCursorValue(cursor, "base_entity_id"));
            taskDetails.setTaskCode(getCursorValue(cursor, "code"));
            taskDetails.setTaskStatus(getCursorValue(cursor, "task_status"));
            taskDetails.setBusinessStatus(getCursorValue(cursor, "business_status"));

            taskDetails.setPhoneNumbers(numbers.get(getCursorValue(cursor, "base_entity_id")));

            Location location = new Location((String) null);
            String longitude = getCursorValue(cursor, "longitude");
            String latitude = getCursorValue(cursor, "latitude");

            if (StringUtils.isNotBlank(longitude) && StringUtils.isNotBlank(latitude)) {
                location.setLatitude(Double.parseDouble(latitude));
                location.setLongitude(Double.parseDouble(longitude));
            } else if (lastLocation != null) {
                location.setLatitude(lastLocation.getLatitude());
                location.setLongitude(lastLocation.getLongitude());
            } else {
                location.setLatitude(0d);
                location.setLongitude(0d);
            }


            if (Constants.BusinessStatus.COMPLETE.equals(taskDetails.getBusinessStatus())) {
                taskDetails.setAggregateBusinessStatus(Constants.BusinessStatus.COMPLETE);
            } else if (Constants.BusinessStatus.VISITED_PARTIALLY_TREATED.equals(taskDetails.getBusinessStatus())) {
                taskDetails.setAggregateBusinessStatus(Constants.BusinessStatus.INCOMPLETE);
            } else if (Constants.BusinessStatus.NOT_VISITED.equals(taskDetails.getBusinessStatus())) {
                taskDetails.setAggregateBusinessStatus(Constants.BusinessStatus.NOT_VISITED);
            } else if (Constants.BusinessStatus.INELIGIBLE.equals(taskDetails.getBusinessStatus())) {
                taskDetails.setAggregateBusinessStatus(Constants.BusinessStatus.COMPLETE);
            } else if (Constants.BusinessStatus.VISITED_NOT_TREATED.equals(taskDetails.getBusinessStatus())) {
                taskDetails.setAggregateBusinessStatus(Constants.BusinessStatus.INCOMPLETE);
            } else if (Constants.BusinessStatus.INCLUDED_IN_ANOTHER_HOUSEHOLD.equals(taskDetails.getBusinessStatus())) {
                taskDetails.setAggregateBusinessStatus(Constants.BusinessStatus.COMPLETE);
            }

            taskDetails.setLocation(location);

            calculateDistance(taskDetails, location, lastLocation, operationalAreaCenter);

            taskDetails.setFamilyMemberNames(memberDetails.get(getCursorValue(cursor, "structure_id", "")));

            return taskDetails;
        };

        List<TaskDetails> result = readData(sql, dataMap);

        return (result == null ? new ArrayList<>() : result);
    }

    private void calculateDistance(TaskDetails task, Location location, Location lastLocation, Location operationalAreaCenter) {
        if (lastLocation != null) {
            task.setDistanceFromUser(location.distanceTo(lastLocation));
        } else {
            if (operationalAreaCenter != null)
                task.setDistanceFromUser(location.distanceTo(operationalAreaCenter));
            task.setDistanceFromCenter(true);
        }
    }

    public List<StructureTaskDetails> getFamilyStructureTasks(String familyBaseID) {
        String sql = "select base_entity_id , dob , first_name , last_name , task._id, task.status , task.business_status , task.code " +
                "from ec_family_member " +
                "inner join task on task.for = ec_family_member.base_entity_id and task.code = '" + Constants.Intervention.NTD_MDA_DISPENSE + "' " +
                "where relational_id = '" + familyBaseID + "'";

        DataMap<StructureTaskDetails> dataMap = cursor -> {
            StructureTaskDetails details = new StructureTaskDetails(getCursorValue(cursor, "_id", ""));
            details.setBusinessStatus(getCursorValue(cursor, "business_status"));
            details.setTaskStatus(getCursorValue(cursor, "status"));
            details.setFamilyMemberNames(getCursorValue(cursor, "first_name") + " " + getCursorValue(cursor, "last_name") + ", " + getAge(getCursorValue(cursor, "dob")));
            details.setTaskCode(getCursorValue(cursor, "code"));
            details.setPersonBaseEntityId(getCursorValue(cursor, "base_entity_id"));
            return details;
        };

        List<StructureTaskDetails> result = readData(sql, dataMap);
        return (result == null ? new ArrayList<>() : result);
    }

    public Task getCurrentTask(String baseEntityID, String taskType) {
        String taskSQL = "select _id from task where for = '" + baseEntityID + "' and code = '" + taskType + "' order by  authored_on desc limit 1";
        DataMap<String> dataMap = cursor -> getCursorValue(cursor, "_id");

        String taskId = AbstractDao.readSingleValue(taskSQL, dataMap);
        return RevealApplication.getInstance().getTaskRepository().getTaskByIdentifier(taskId);
    }
}
