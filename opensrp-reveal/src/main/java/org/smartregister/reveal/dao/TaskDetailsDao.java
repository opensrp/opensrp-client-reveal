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
import java.util.List;

import static org.smartregister.reveal.util.FamilyJsonFormUtils.getAge;

public class TaskDetailsDao extends AbstractDao {

    public static TaskDetailsDao getInstance() {
        return new TaskDetailsDao();
    }

    public List<TaskDetails> fetchTaskFamilies(String operationalAreaId, String planId, Location lastLocation, Location operationalAreaCenter) {

        String sql = "select task.structure_id , ec_family.base_entity_id , qr_code , " +
                "(case when max(ifnull(ec_family.nsac,0),ifnull(total_tasks,0)) = 0 then 'Complete' " +
                "when ifnull(pending_tasks,0) < max(ifnull(ec_family.nsac,0),ifnull(total_tasks,0)) then 'Partial' " +
                "else 'Not Visited' end) task_status , ec_family.first_name fam_name , structure.latitude , structure.longitude " +
                "from task  " +
                "inner join structure on structure._id = task.structure_id " +
                "left join structure_eligibility on structure_eligibility.structure_id = task.structure_id " +
                "left join ( " +
                " select structure_id , " +
                " count(*) total_tasks , sum(case when business_status = 'Drug Administered' then 0 else 1 end) pending_tasks " +
                " from task where code = '" + Constants.Intervention.NTD_MDA_DISPENSE + "' " +
                " and plan_id = '" + planId + "' and group_id = '" + operationalAreaId + "' " +
                " group by structure_id " +
                ") dispenses on dispenses.structure_id = task.structure_id " +
                "left join ec_family on ec_family.structure_id = task.structure_id " +
                "where task.code = '" + Constants.Intervention.REGISTER_FAMILY + "' and task.business_status = 'Complete' " +
                "and task.plan_id = '" + planId + "' and task.group_id = '" + operationalAreaId + "'";

        DataMap<TaskDetails> dataMap = cursor -> {
            TaskDetails taskDetails = new TaskDetails(getCursorValue(cursor, "base_entity_id", ""));
            taskDetails.setStructureName(getCursorValue(cursor, "fam_name") + " Family");
            taskDetails.setFamilyBaseEntityID(getCursorValue(cursor, "base_entity_id"));
            taskDetails.setQrCode(getCursorValue(cursor, "qr_code"));
            taskDetails.setTaskCode(Constants.Intervention.REGISTER_FAMILY);
            taskDetails.setTaskStatus(getCursorValue(cursor, "task_status"));
            taskDetails.setBusinessStatus(getCursorValue(cursor, "task_status"));

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

            taskDetails.setLocation(location);

            calculateDistance(taskDetails, location, lastLocation, operationalAreaCenter);

            return taskDetails;
        };

        List<TaskDetails> result = readData(sql, dataMap);

        return (result == null ? new ArrayList<>() : result);
    }


    public List<TaskDetails> fetchStructures(String operationalAreaId, String planId, Location lastLocation, Location operationalAreaCenter) {
        String sql =
                "select ec_family.base_entity_id , ec_family.structure_id , ec_family.first_name fam_name , structure.latitude , structure.longitude, ifnull(task.business_status,'Not Visited') business_status , task.status task_status , '" + Constants.Intervention.REGISTER_FAMILY + "' code " +
                        "from ec_family " +
                        "inner join structure on ec_family.structure_id = structure._id " +
                        "inner join task on task.structure_id = ec_family.structure_id and task.code = '" + Constants.Intervention.STRUCTURE_VISITED + "'  " +
                        "and task.plan_id = '" + planId + "' and  task.group_id = '" + operationalAreaId + "' " +
                        "union  " +
                        "select null base_entity_id , structure._id structure_id , structure.name , structure.latitude , structure.longitude, ifnull(task.business_status,'Not Visited') , task.status , '" + Constants.Intervention.REGISTER_FAMILY + "' code " +
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


        DataMap<TaskDetails> dataMap = cursor -> {
            TaskDetails taskDetails = new TaskDetails(getCursorValue(cursor, "structure_id", ""));

            String familyName = getCursorValue(cursor, "fam_name");
            if (StringUtils.isNotBlank(familyName))
                taskDetails.setStructureName(familyName + " Family");
            taskDetails.setFamilyBaseEntityID(getCursorValue(cursor, "base_entity_id"));
            taskDetails.setTaskCode(getCursorValue(cursor, "code"));
            taskDetails.setTaskStatus(getCursorValue(cursor, "task_status"));
            taskDetails.setBusinessStatus(getCursorValue(cursor, "business_status"));

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

            taskDetails.setLocation(location);

            calculateDistance(taskDetails, location, lastLocation, operationalAreaCenter);

            return taskDetails;
        };

        List<TaskDetails> result = readData(sql, dataMap);

        return (result == null ? new ArrayList<>() : result);
    }

    public List<TaskDetails> fetchFloatingFamilies(String operationalAreaId, String planId, Location lastLocation, Location operationalAreaCenter) {

        String sql = "select base_entity_id , first_name fam_name ,  " +
                "(case   " +
                "when total_tasks is null then 'Not Visited'  " +
                "when max(ifnull(ec_family.nsac,0),ifnull(total_tasks,0)) = 0 then 'Complete'   " +
                "when ifnull(pending_tasks,0) < max(ifnull(ec_family.nsac,0),ifnull(total_tasks,0)) then 'Partial'   " +
                "else 'Not Visited' end) task_status  " +
                "from ec_family  " +
                "left join (  " +
                "  select ec_family_member.relational_id ,  " +
                "    count(*) total_tasks , sum(case when business_status = 'Drug Administered' then 0 else 1 end) pending_tasks  " +
                "  from ec_family_member  " +
                "  inner join task on ec_family_member.base_entity_id = task.for and task.code = '" + Constants.Intervention.NTD_MDA_DISPENSE + "'  " +
                "  where task.plan_id = '" + planId + "' and task.group_id = '" + operationalAreaId + "'  " +
                ") dispenses on ec_family.base_entity_id = dispenses.relational_id  " +
                "where structure_id is null or structure_id = operational_area_id";

        DataMap<TaskDetails> dataMap = cursor -> {
            TaskDetails taskDetails = new TaskDetails(getCursorValue(cursor, "base_entity_id", ""));
            taskDetails.setStructureName(getCursorValue(cursor, "fam_name") + " Family");
            taskDetails.setFamilyBaseEntityID(getCursorValue(cursor, "base_entity_id"));
            taskDetails.setTaskCode(Constants.InterventionType.FLOATING_FAMILY);
            taskDetails.setTaskStatus(getCursorValue(cursor, "task_status"));
            taskDetails.setBusinessStatus(getCursorValue(cursor, "task_status"));

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

    public List<TaskDetails> fetchUnRegisteredStructures(String operationalAreaId, String planId, Location lastLocation, Location operationalAreaCenter) {

        String sql = "select _id structure_id , name ,  latitude , longitude  " +
                "from structure  " +
                "where plan_id = '" + planId + "' and group_id = '" + operationalAreaId + "'  \"  " +
                " and _id not in (  " +
                "  select structure_id from task where code = 'Structure Visited'   " +
                "  and plan_id = '" + planId + "' and group_id = '" + operationalAreaId + "'  " +
                ")";

        DataMap<TaskDetails> dataMap = cursor -> {
            TaskDetails taskDetails = new TaskDetails(getCursorValue(cursor, "structure_id", ""));
            taskDetails.setStructureName(getCursorValue(cursor, "name"));
            taskDetails.setTaskCode(Constants.Intervention.REGISTER_FAMILY);
            taskDetails.setTaskStatus("Not Visited");
            taskDetails.setBusinessStatus("Not Visited");

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

            taskDetails.setLocation(location);

            calculateDistance(taskDetails, location, lastLocation, operationalAreaCenter);

            return taskDetails;
        };

        List<TaskDetails> result = readData(sql, dataMap);

        return (result == null ? new ArrayList<>() : result);
    }

    public List<TaskDetails> fetchFixedStructures(String operationalAreaId, String planId, Location lastLocation, Location operationalAreaCenter) {

        String sql = "select structure_id , business_status , structure.latitude , structure.longitude , structure.name " +
                "from task inner join structure on structure._id = task.structure_id " +
                "where business_status in ('Included in another household', 'Ineligible', 'Visited, Denied consent')  " +
                "and code in ('" + Constants.Intervention.STRUCTURE_VISITED + "', '" + Constants.Intervention.REGISTER_FAMILY + "')  " +
                "and plan_id = '" + planId + "' and group_id = '" + operationalAreaId + "'";

        DataMap<TaskDetails> dataMap = cursor -> {
            TaskDetails taskDetails = new TaskDetails(getCursorValue(cursor, "structure_id", ""));
            taskDetails.setStructureName(getCursorValue(cursor, "name"));
            taskDetails.setTaskCode(Constants.Intervention.REGISTER_FAMILY);
            taskDetails.setTaskStatus(getCursorValue(cursor, "business_status"));
            taskDetails.setBusinessStatus(getCursorValue(cursor, "business_status"));


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
