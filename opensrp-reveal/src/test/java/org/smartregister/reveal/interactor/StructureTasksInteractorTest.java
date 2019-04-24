package org.smartregister.reveal.interactor;

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
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.repository.StructureRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.TestingUtils;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.Executors;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.family.util.DBConstants.KEY.DOB;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CODE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FOR;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;

/**
 * Created by samuelgithengi on 4/24/19.
 */
public class StructureTasksInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private StructureTasksContract.Presenter presenter;

    @Mock
    private SQLiteDatabase database;

    @Mock
    private StructureRepository structureRepository;

    @Captor
    private ArgumentCaptor<List<StructureTaskDetails>> taskDetailsArgumentCaptor;

    private StructureTasksInteractor interactor;

    @Before
    public void setUp() {
        AppExecutors appExecutors = new AppExecutors(Executors.newSingleThreadExecutor(),
                Executors.newSingleThreadExecutor(), Executors.newSingleThreadExecutor());
        interactor = new StructureTasksInteractor(presenter, appExecutors, database, structureRepository);
    }

    @Test
    public void testFindTasks() {
        String campaign = UUID.randomUUID().toString();
        String structure = UUID.randomUUID().toString();
        String taskQuery = "Select task._id as _id , task._id , task.code , task.for , task.business_status , task.status FROM task WHERE for=? AND campaign_id=? ";
        String memberQuery = "Select structure._id as _id , task._id , task.code , task.for , task.business_status , task.status , printf('%s %s %s',first_name,middle_name,last_name) AS name , dob , structure_id FROM structure  LEFT JOIN ec_family_member ON ec_family_member.structure_id = structure._id   LEFT JOIN task ON task.for = ec_family_member.base_entity_id  WHERE structure._id=? AND campaign_id=? ";
        when(database.rawQuery(taskQuery, new String[]{structure, campaign})).thenReturn(createCursor());
        when(database.rawQuery(memberQuery, new String[]{structure, campaign})).thenReturn(createMemberCursor());

        interactor.findTasks(structure, campaign);

        verify(database, timeout(ASYNC_TIMEOUT)).rawQuery(taskQuery, new String[]{structure, campaign});
        verify(database, timeout(ASYNC_TIMEOUT)).rawQuery(memberQuery, new String[]{structure, campaign});

        verify(presenter, timeout(ASYNC_TIMEOUT)).onTasksFound(taskDetailsArgumentCaptor.capture());

        assertEquals(2, taskDetailsArgumentCaptor.getValue().size());

        StructureTaskDetails irsTask = taskDetailsArgumentCaptor.getValue().get(0);
        assertEquals("task_id_1", irsTask.getTaskId());
        assertEquals(Intervention.IRS, irsTask.getTaskCode());
        assertEquals("434343", irsTask.getTaskEntity());
        assertEquals(BusinessStatus.NOT_SPRAYED, irsTask.getBusinessStatus());
        assertEquals(Task.TaskStatus.COMPLETED.name(), irsTask.getTaskStatus());

        StructureTaskDetails memberTask = taskDetailsArgumentCaptor.getValue().get(1);
        assertEquals("task_id_2", memberTask.getTaskId());
        assertEquals(Intervention.BLOOD_SCREENING, memberTask.getTaskCode());
        assertEquals("w08989-ejkjkj-eere", memberTask.getTaskEntity());
        assertEquals(BusinessStatus.NOT_VISITED, memberTask.getBusinessStatus());
        assertEquals(Task.TaskStatus.READY.name(), memberTask.getTaskStatus());
        String age = org.smartregister.reveal.util.Utils.getAge("1982-01-01T03:00:00.000+03:00");
        assertEquals("Charity Otala, " + age, memberTask.getTaskName());
        assertEquals("434343", memberTask.getStructureId());


    }


    @Test
    public void testGetStructure() {
        StructureTaskDetails taskDetails = TestingUtils.getStructureTaskDetails();
        Location location = new Location();
        when(structureRepository.getLocationById(taskDetails.getTaskEntity())).thenReturn(location);
        interactor.getStructure(taskDetails);
        verify(structureRepository, timeout(ASYNC_TIMEOUT)).getLocationById(taskDetails.getTaskEntity());
        verify(presenter, timeout(ASYNC_TIMEOUT)).onStructureFound(location, taskDetails);
    }


    @Test
    public void testGetStructureForMemberTask() {
        StructureTaskDetails taskDetails = TestingUtils.getStructureTaskDetails();
        taskDetails.setTaskCode(Intervention.BLOOD_SCREENING);
        taskDetails.setStructureId(UUID.randomUUID().toString());
        Location location = new Location();
        when(structureRepository.getLocationById(taskDetails.getStructureId())).thenReturn(location);
        interactor.getStructure(taskDetails);
        verify(structureRepository, timeout(ASYNC_TIMEOUT)).getLocationById(taskDetails.getStructureId());
        verify(presenter, timeout(ASYNC_TIMEOUT)).onStructureFound(location, taskDetails);
    }

    private Cursor createCursor() {
        MatrixCursor cursor = new MatrixCursor(new String[]{
                ID,
                CODE,
                FOR,
                BUSINESS_STATUS,
                STATUS
        });
        cursor.addRow(new Object[]{
                "task_id_1",
                Intervention.IRS,
                434343,
                BusinessStatus.NOT_SPRAYED,
                Task.TaskStatus.COMPLETED
        });
        return cursor;
    }


    private Cursor createMemberCursor() {
        MatrixCursor cursor = new MatrixCursor(new String[]{
                ID,
                CODE,
                FOR,
                BUSINESS_STATUS,
                STATUS,
                NAME,
                DOB,
                STRUCTURE_ID
        });
        cursor.addRow(new Object[]{
                "task_id_2",
                Intervention.BLOOD_SCREENING,
                "w08989-ejkjkj-eere",
                BusinessStatus.NOT_VISITED,
                Task.TaskStatus.READY,
                "Charity Otala",
                "1982-01-01T03:00:00.000+03:00",
                434343
        });
        return cursor;
    }
}
