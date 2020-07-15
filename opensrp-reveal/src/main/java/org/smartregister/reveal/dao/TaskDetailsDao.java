package org.smartregister.reveal.dao;

import android.content.Context;
import android.location.Location;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.dao.AbstractDao;
import org.smartregister.domain.Task;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants;

import java.util.ArrayList;
import java.util.List;

import static org.smartregister.reveal.util.FamilyJsonFormUtils.getAge;

public class TaskDetailsDao extends AbstractDao {

    public static TaskDetailsDao getInstance() {
        return new TaskDetailsDao();
    }

    public List<TaskDetails> getTasks(String operationalAreaID, Location lastLocation, Location operationalAreaCenter) {

        String sql = "select ec_family.base_entity_id , structure._id structure_id , ec_family.first_name , structure.latitude , structure.longitude , ifnull(completed_tasks,0) completed_tasks , ifnull(total_tasks, 0) total_tasks " +
                "from structure  " +
                "inner join ec_family on ec_family.structure_id = structure._id " +
                "left join ( " +
                " select ec_family.base_entity_id, sum(case when task.status = 'COMPLETED' then 0 else 1 end) completed_tasks , count(*) total_tasks " +
                " from task  " +
                " inner join ec_family_member on task.for = ec_family_member.base_entity_id " +
                " inner join ec_family on ec_family.base_entity_id = ec_family_member.relational_id " +
                " group by ec_family.base_entity_id " +
                ") family_member on family_member.base_entity_id =  ec_family.base_entity_id " +
                "where parent_id = '" + operationalAreaID + "' and first_name is not null " +
                "order by (case when ec_family.base_entity_id is null then 1 else 0 end) , first_name  ";

        DataMap<TaskDetails> dataMap = cursor -> {
            TaskDetails taskDetails = new TaskDetails(getCursorValue(cursor, "base_entity_id", ""));
            taskDetails.setTaskCode(Constants.Intervention.NTD_COMMUNITY);
            taskDetails.setStructureId(getCursorValue(cursor, "structure_id"));
            taskDetails.setStructureName(getCursorValue(cursor, "first_name") + " Family");

            Integer total = getCursorIntValue(cursor, "total_tasks", 0);
            Integer completed = getCursorIntValue(cursor, "completed_tasks", 0);

            if (total == 0) {
                taskDetails.setBusinessStatus(Constants.BusinessStatus.NO_TASK);
            } else if (total > completed && completed > 0) {
                taskDetails.setBusinessStatus(Constants.BusinessStatus.PARTIALLY_RECEIVED);
            } else if (total > completed && completed == 0) {
                taskDetails.setBusinessStatus(Constants.BusinessStatus.NOT_VISITED);
            } else if (total.equals(completed)) {
                taskDetails.setBusinessStatus(Constants.BusinessStatus.COMPLETE);
            }

            Long latitude = getCursorLongValue(cursor, "latitude");
            Long longitude = getCursorLongValue(cursor, "longitude");

            Location location = new Location((String) null);
            if (latitude != null)
                location.setLatitude(latitude.doubleValue());
            if (longitude != null)
                location.setLongitude(longitude.doubleValue());

            taskDetails.setLocation(location);

            calculateDistance(taskDetails, location, lastLocation, operationalAreaCenter);

            return taskDetails;
        };

        List<TaskDetails> result = readData(sql, dataMap);

        return (result == null ? new ArrayList<>() : result);
    }


    private void calculateDistance(TaskDetails task, Location location, Location lastLocation, Location operationalAreaCenter) {
        if (lastLocation != null) {
            task.setDistanceFromUser(location.distanceTo(lastLocation));
        } else {
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
