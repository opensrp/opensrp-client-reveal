package org.smartregister.reveal.dao;

import android.location.Location;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.dao.AbstractDao;
import org.smartregister.domain.Task;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants;
import org.smartregister.util.QueryComposer;

import java.util.ArrayList;
import java.util.List;

import static org.smartregister.reveal.util.FamilyJsonFormUtils.getAge;

public class TaskDetailsDao extends AbstractDao {

    public static TaskDetailsDao getInstance() {
        return new TaskDetailsDao();
    }

    public List<TaskDetails> getTasks(String operationalAreaID, Location lastLocation, Location operationalAreaCenter) {

        String sql = "" +
                "select ec_family.base_entity_id , summary.total_tasks , summary.completed_tasks , ec_family.first_name , structure_eligibility.qr_code " +
                "from ec_family " +
                "left join structure_eligibility on structure_eligibility.structure_id = ec_family.structure_id " +
                "left join ( " +
                " select sum(case when task.status = 'COMPLETED' then 1 else 0 end) completed_tasks , count(*) total_tasks , ec_family_member.relational_id " +
                " from task " +
                " inner join ec_family_member on ec_family_member.base_entity_id = task.for " +
                " group by ec_family_member.relational_id " +
                ") summary " +
                "on summary.relational_id = ec_family.base_entity_id " +
                "where ec_family.operational_area_id = '" + operationalAreaID + "' ";

        // add floating families

        DataMap<TaskDetails> dataMap = cursor -> {
            TaskDetails taskDetails = new TaskDetails(getCursorValue(cursor, "base_entity_id", ""));
            taskDetails.setStructureName(getCursorValue(cursor, "first_name") + " Family");
            taskDetails.setFamilyBaseEntityID(getCursorValue(cursor, "base_entity_id"));
            taskDetails.setQrCode(getCursorValue(cursor, "qr_code"));
            taskDetails.setTaskCode(Constants.Intervention.NTD_COMMUNITY);
            taskDetails.setTaskStatus(Task.TaskStatus.COMPLETED.name());

            Integer total = getCursorIntValue(cursor, "total_tasks", 0);
            Integer completed = getCursorIntValue(cursor, "completed_tasks", 0);
            taskDetails.setTaskCount(total);

            if (total == 0) {
                taskDetails.setBusinessStatus(Constants.BusinessStatus.NO_TASK);
            } else if (total > completed) {
                taskDetails.setBusinessStatus(Constants.BusinessStatus.VISITED_PARTIALLY_TREATED);
            } else if (total.equals(completed)) {
                taskDetails.setBusinessStatus(Constants.BusinessStatus.COMPLETE);
            }

            Location location = new Location((String) null);
            if (lastLocation != null) {
                location.setLatitude(lastLocation.getLatitude());
                location.setLongitude(lastLocation.getLongitude());
            } else {
                location.setLatitude(0d);
                location.setLongitude(0d);
            }

            taskDetails.setLocation(location);

            calculateDistance(taskDetails, location, lastLocation, operationalAreaCenter);

            return taskDetails;
        };

        List<TaskDetails> result = readData(sql, dataMap);

        return (result == null ? new ArrayList<>() : result);
    }

    public List<TaskDetails> getUnRegisteredStructures(String operationalAreaID, Location lastLocation, Location operationalAreaCenter) {

        String sql = "SELECT structure._id , structure.name , structure.latitude , structure.longitude , structure_eligibility.qr_code " +
                "from structure " +
                "left join structure_eligibility on structure_eligibility.structure_id = structure._id " +
                "where structure.parent_id = '" + operationalAreaID + "' " +
                "and structure._id not in (select structure_id from ec_family where structure_id is not null) ";

        DataMap<TaskDetails> dataMap = cursor -> {
            TaskDetails taskDetails = new TaskDetails(getCursorValue(cursor, "_id", ""));
            taskDetails.setStructureName(getCursorValue(cursor, "name"));
            taskDetails.setStructureId(getCursorValue(cursor, "_id"));
            taskDetails.setQrCode(getCursorValue(cursor, "qr_code"));
            taskDetails.setTaskStatus(Task.TaskStatus.READY.name());
            taskDetails.setBusinessStatus(Constants.BusinessStatus.NOT_VISITED);

            Location location = new Location((String) null);

            String longitude = getCursorValue(cursor, "longitude");
            String latitude = getCursorValue(cursor, "latitude");

            if (StringUtils.isNotBlank(longitude) && StringUtils.isNotBlank(latitude)) {
                location.setLatitude(Long.parseLong(latitude));
                location.setLongitude(Long.parseLong(longitude));
            } else {
                location.setLatitude(0d);
                location.setLongitude(0d);
            }

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
