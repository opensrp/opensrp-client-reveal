package org.smartregister.reveal.interactor;

import android.location.Location;
import android.support.v4.util.Pair;

import net.sqlcipher.Cursor;
import net.sqlcipher.MatrixCursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.domain.Task;
import org.smartregister.repository.StructureRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.TaskRegisterFragmentContract;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.smartregister.domain.Task.TaskStatus.CANCELLED;
import static org.smartregister.family.util.DBConstants.KEY.FIRST_NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CODE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.COMPLETED_TASK_COUNT;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FAMILY_NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FOR;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.GROUPED_STRUCTURE_TASK_CODE_AND_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LATITUDE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LONGITUDE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NOT_SRAYED_OTHER_REASON;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NOT_SRAYED_REASON;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.SPRAY_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_COUNT;

/**
 * Created by samuelgithengi on 3/27/19.
 */
public class TaskRegisterFragmentInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private TaskRegisterFragmentContract.Presenter presenter;

    @Mock
    private SQLiteDatabase database;

    @Mock
    private StructureRepository structureRepository;

    @Captor
    private ArgumentCaptor<List<TaskDetails>> taskListCaptor;

    @Captor
    private ArgumentCaptor<Integer> structuresCaptor;

    private TaskRegisterFragmentInteractor interactor;

    private String groupId;
    private String planId;
    private String mainSelectQuery;
    private String nonRegisteredStructureTasksQuery;
    private String groupedRegisteredStructureTasksSelectQuery;
    private String bccSelectQuery;

    @Before
    public void setUp() {
        interactor = new TaskRegisterFragmentInteractor(presenter, 70f);
        Whitebox.setInternalState(interactor, "structureRepository", structureRepository);
        Whitebox.setInternalState(interactor, "database", database);
        groupId = UUID.randomUUID().toString();
        planId = UUID.randomUUID().toString();
        mainSelectQuery = "Select task._id as _id , task._id , task.code , task.for , task.business_status , task.status , structure.latitude , structure.longitude , structure.name , sprayed_structures.structure_name , sprayed_structures.family_head_name , sprayed_structures.spray_status , sprayed_structures.not_sprayed_reason , sprayed_structures.not_sprayed_other_reason , structure._id AS structure_id , ec_family.first_name FROM task  JOIN structure ON task.for = structure._id   LEFT JOIN sprayed_structures ON task.for = sprayed_structures.base_entity_id   LEFT JOIN ec_family ON structure._id = ec_family.structure_id  WHERE task.group_id = ? AND task.plan_id = ? AND status != ? ";
        groupId = UUID.randomUUID().toString();
        planId = UUID.randomUUID().toString();
        nonRegisteredStructureTasksQuery = "Select task._id as _id , task._id , task.code , task.for , task.business_status , task.status , structure.latitude , structure.longitude , structure.name , sprayed_structures.structure_name , sprayed_structures.family_head_name , sprayed_structures.spray_status , sprayed_structures.not_sprayed_reason , sprayed_structures.not_sprayed_other_reason , structure._id AS structure_id , ec_family.first_name FROM task  JOIN structure ON task.for = structure._id   LEFT JOIN sprayed_structures ON task.for = sprayed_structures.base_entity_id   LEFT JOIN ec_family ON structure._id = ec_family.structure_id  WHERE task.group_id = ? AND task.plan_id = ? AND status != ?   AND ec_family.structure_id IS NULL";
        groupedRegisteredStructureTasksSelectQuery = " SELECT grouped_tasks.*, SUM(CASE WHEN grouped_tasks.status='COMPLETED' THEN 1 ELSE 0 END) AS completed_task_count , COUNT(grouped_tasks._id) AS task_count, GROUP_CONCAT(grouped_tasks.code || \"-\" || grouped_tasks.business_status) AS grouped_structure_task_code_and_status FROM ( Select task._id as _id , task._id , task.code , task.for , task.business_status , task.status , structure.latitude , structure.longitude , structure.name , sprayed_structures.structure_name , sprayed_structures.family_head_name , sprayed_structures.spray_status , sprayed_structures.not_sprayed_reason , sprayed_structures.not_sprayed_other_reason , structure._id AS structure_id , ec_family.first_name FROM task  JOIN structure ON task.structure_id = structure._id   JOIN ec_family ON structure._id = ec_family.structure_id  COLLATE NOCASE  LEFT JOIN sprayed_structures ON task.for = sprayed_structures.base_entity_id  WHERE task.group_id = ? AND task.plan_id = ? AND status != ?  ) AS grouped_tasks GROUP BY grouped_tasks.structure_id ";
        bccSelectQuery = "SELECT * FROM task WHERE for = ? AND plan_id = ? AND status != ? AND code ='BCC'";
    }

    @Test
    public void testFindTasksWithNullParams() {
        interactor.findTasks(null, null, null, "House");
        verify(presenter).onTasksFound(null, 0);
        verifyNoMoreInteractions(presenter);
        verifyNoMoreInteractions(database);
    }


    @Test
    public void testFindTasksWithOperationalAreaLocation() {
        PreferencesUtil.getInstance().setCurrentPlan("FI_2019_TV01_IRS");
        Pair<String, String[]> pair = new Pair<>("task.group_id = ? AND task.plan_id = ? AND status != ?", new String[]{groupId, planId, CANCELLED.name()});
        Location center = new Location("Test");
        center.setLatitude(-14.152197);
        center.setLongitude(32.643570);
        when(database.rawQuery(mainSelectQuery, new String[]{groupId, planId, CANCELLED.name()})).thenReturn(createCursor());
        when(database.rawQuery(bccSelectQuery, new String[]{groupId, planId, CANCELLED.name()})).thenReturn(createCursor());
        interactor.findTasks(pair, null, center, "House");
        verify(database, timeout(ASYNC_TIMEOUT)).rawQuery(mainSelectQuery, new String[]{groupId, planId, CANCELLED.name()});
        verify(database, timeout(ASYNC_TIMEOUT)).rawQuery(bccSelectQuery, new String[]{groupId, planId, CANCELLED.name()});
        verify(presenter, timeout(ASYNC_TIMEOUT)).onTasksFound(taskListCaptor.capture(), structuresCaptor.capture());
        verifyNoMoreInteractions(presenter);
        assertEquals(2, taskListCaptor.getValue().size());
        TaskDetails taskDetails = taskListCaptor.getValue().get(0);
        assertEquals("task_id_1", taskDetails.getTaskId());
        assertEquals(Intervention.IRS, taskDetails.getTaskCode());
        assertEquals(BusinessStatus.NOT_SPRAYED, taskDetails.getBusinessStatus());
        assertEquals(Task.TaskStatus.COMPLETED.name(), taskDetails.getTaskStatus());
        assertEquals(-14.15191915, taskDetails.getLocation().getLatitude(), 0.00001);
        assertEquals(32.64302015, taskDetails.getLocation().getLongitude(), 0.00001);
        assertEquals("Structure 976", taskDetails.getStructureName());
        assertEquals("Ali House", taskDetails.getFamilyName());
        assertEquals("Partial Spray", taskDetails.getSprayStatus());
        assertEquals("Ali House", taskDetails.getFamilyName());
        assertEquals("Not Completed", taskDetails.getTaskDetails());
        assertEquals(66.850830078125, taskDetails.getDistanceFromUser(), 0.00001);
        assertEquals(2, structuresCaptor.getValue().intValue());
    }

    @Test
    public void testFindTasksWithUserLocation() {
        PreferencesUtil.getInstance().setCurrentPlan("FI_2019_TV01_IRS");
        Pair<String, String[]> pair = new Pair<>("task.group_id = ? AND task.plan_id = ? AND status != ?", new String[]{groupId, planId, CANCELLED.name()});
        Location userLocation = new Location("Test");
        userLocation.setLatitude(-14.987197);
        userLocation.setLongitude(32.076570);
        when(database.rawQuery(mainSelectQuery, new String[]{groupId, planId, CANCELLED.name()})).thenReturn(createCursor());
        when(database.rawQuery(bccSelectQuery, new String[]{groupId, planId, CANCELLED.name()})).thenReturn(createCursor());
        interactor.findTasks(pair, userLocation, null, "House");
        verify(database, timeout(ASYNC_TIMEOUT)).rawQuery(mainSelectQuery, new String[]{groupId, planId, CANCELLED.name()});
        verify(database, timeout(ASYNC_TIMEOUT)).rawQuery(bccSelectQuery, new String[]{groupId, planId, CANCELLED.name()});
        verify(presenter, timeout(ASYNC_TIMEOUT)).onTasksFound(taskListCaptor.capture(), structuresCaptor.capture());
        verifyNoMoreInteractions(presenter);
        assertEquals(2, taskListCaptor.getValue().size());
        TaskDetails taskDetails = taskListCaptor.getValue().get(0);
        assertEquals("task_id_1", taskDetails.getTaskId());
        assertEquals(Intervention.IRS, taskDetails.getTaskCode());
        assertEquals(BusinessStatus.NOT_SPRAYED, taskDetails.getBusinessStatus());
        assertEquals(Task.TaskStatus.COMPLETED.name(), taskDetails.getTaskStatus());
        assertEquals(-14.15191915, taskDetails.getLocation().getLatitude(), 0.00001);
        assertEquals(32.64302015, taskDetails.getLocation().getLongitude(), 0.00001);
        assertEquals("Structure 976", taskDetails.getStructureName());
        assertEquals("Ali House", taskDetails.getFamilyName());
        assertEquals("Partial Spray", taskDetails.getSprayStatus());
        assertEquals("Ali House", taskDetails.getFamilyName());
        assertEquals("Not Completed", taskDetails.getTaskDetails());
        assertEquals(110757.984375, taskDetails.getDistanceFromUser(), 0.00001);
        assertEquals(0, structuresCaptor.getValue().intValue());
    }

    @Test
    public void testCalculateDistanceFromUserWithNullTasks() {
        interactor.calculateDistanceFromUser(null, null);
        verifyNoMoreInteractions(presenter);

    }

    @Test
    public void testCalculateDistanceFromUser() {
        Location center = new Location("Test");
        center.setLatitude(-14.152197);
        center.setLongitude(32.643570);
        List<TaskDetails> tasks = new ArrayList<>();
        TaskDetails task1 = TestingUtils.getTaskDetails();
        Location location1 = new Location("Test");
        location1.setLongitude(-14.152197);
        location1.setLongitude(32.643232);
        task1.setLocation(location1);

        tasks.add(task1);

        TaskDetails task2 = TestingUtils.getTaskDetails();
        task2.setTaskId("task_id_2");
        task2.setLocation(center);
        tasks.add(task2);

        interactor.calculateDistanceFromUser(tasks, center);


        verify(presenter, timeout(ASYNC_TIMEOUT)).onTasksFound(taskListCaptor.capture(), structuresCaptor.capture());
        List<TaskDetails> orderedTasks = taskListCaptor.getValue();

        assertEquals(1, orderedTasks.indexOf(task1));
        assertEquals(0, orderedTasks.indexOf(task2));
        verifyNoMoreInteractions(presenter);

    }

    @Test
    public void testFindTasksWithTaskGrouping() {
        PreferencesUtil.getInstance().setCurrentPlan("FI_2019_TV01_Focus");
        Pair<String, String[]> pair = new Pair<>("task.group_id = ? AND task.plan_id = ? AND status != ?", new String[]{groupId, planId, CANCELLED.name()});
        Location userLocation = new Location("Test");
        userLocation.setLatitude(-14.987197);
        userLocation.setLongitude(32.076570);
        when(database.rawQuery(groupedRegisteredStructureTasksSelectQuery, new String[]{groupId, planId, CANCELLED.name()})).thenReturn(createCursor());
        when(database.rawQuery(nonRegisteredStructureTasksQuery, new String[]{groupId, planId, CANCELLED.name()})).thenReturn(createCursor());
        when(database.rawQuery(bccSelectQuery, new String[]{groupId, planId, CANCELLED.name()})).thenReturn(createCursor());
        interactor.findTasks(pair, userLocation, null, "House");
        verify(database, timeout(ASYNC_TIMEOUT)).rawQuery(groupedRegisteredStructureTasksSelectQuery, new String[]{groupId, planId, CANCELLED.name()});
        verify(database, timeout(ASYNC_TIMEOUT)).rawQuery(nonRegisteredStructureTasksQuery, new String[]{groupId, planId, CANCELLED.name()});
        verify(database, timeout(ASYNC_TIMEOUT)).rawQuery(bccSelectQuery, new String[]{groupId, planId, CANCELLED.name()});
        verify(presenter, timeout(ASYNC_TIMEOUT)).onTasksFound(taskListCaptor.capture(), structuresCaptor.capture());
        verifyNoMoreInteractions(presenter);
        assertEquals(3, taskListCaptor.getValue().size());
        TaskDetails taskDetails = taskListCaptor.getValue().get(0);
        assertEquals("task_id_1", taskDetails.getTaskId());
        assertEquals(Intervention.IRS, taskDetails.getTaskCode());
        assertEquals(BusinessStatus.NOT_SPRAYED, taskDetails.getBusinessStatus());
        assertEquals(Task.TaskStatus.COMPLETED.name(), taskDetails.getTaskStatus());
        assertEquals(-14.15191915, taskDetails.getLocation().getLatitude(), 0.00001);
        assertEquals(32.64302015, taskDetails.getLocation().getLongitude(), 0.00001);
        assertEquals("Structure 976", taskDetails.getStructureName());
        assertEquals("Ali House", taskDetails.getFamilyName());
        assertEquals("Partial Spray", taskDetails.getSprayStatus());
        assertEquals("Ali House", taskDetails.getFamilyName());
        assertEquals("Not Completed", taskDetails.getTaskDetails());
        assertEquals(110757.984375, taskDetails.getDistanceFromUser(), 0.00001);
        assertEquals(0, structuresCaptor.getValue().intValue());
    }

    @Test
    public void testGetStructure() {
        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        taskDetails.setStructureId(UUID.randomUUID().toString());
        org.smartregister.domain.Location structure = new org.smartregister.domain.Location();
        structure.setId(UUID.randomUUID().toString());
        when(structureRepository.getLocationById(taskDetails.getStructureId())).thenReturn(structure);
        interactor.getStructure(taskDetails);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onStructureFound(structure, taskDetails);
    }

    private Cursor createCursor() {
        MatrixCursor cursor = new MatrixCursor(new String[]{
                ID,
                CODE,
                FOR,
                BUSINESS_STATUS,
                STATUS,
                LATITUDE,
                LONGITUDE,
                NAME,
                FAMILY_NAME,
                SPRAY_STATUS,
                NOT_SRAYED_REASON,
                NOT_SRAYED_OTHER_REASON,
                STRUCTURE_ID,
                FIRST_NAME,
                TASK_COUNT,
                COMPLETED_TASK_COUNT,
                GROUPED_STRUCTURE_TASK_CODE_AND_STATUS
        });
        cursor.addRow(new Object[]{
                "task_id_1",
                Intervention.IRS,
                434343,
                BusinessStatus.NOT_SPRAYED,
                Task.TaskStatus.COMPLETED,
                -14.15191915,
                32.64302015,
                "Structure 976",
                "Ali",
                "Partial Spray",
                "other",
                "Not Completed",
                434343,
                null,
                1,
                1,
                "BedNet Distribution-Complete"
        });
        return cursor;
    }
}
