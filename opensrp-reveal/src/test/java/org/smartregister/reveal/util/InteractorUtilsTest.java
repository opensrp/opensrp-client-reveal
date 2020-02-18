package org.smartregister.reveal.util;

import net.sqlcipher.Cursor;
import net.sqlcipher.MatrixCursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.model.BaseTaskDetails;
import org.smartregister.reveal.sync.RevealClientProcessor;

import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.BEHAVIOUR_CHANGE_COMMUNICATION;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.BASE_ENTITY_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CASE_CONFIRMATION_FIELD;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FORM_SUBMISSION_ID;
import static org.smartregister.reveal.util.Constants.Intervention.BCC;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;

/**
 * Created by Richard Kareko on 2/18/20.
 */

public class InteractorUtilsTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private TaskRepository taskRepository;

    @Mock
    private EventClientRepository eventClientRepository;

    @Mock
    private RevealClientProcessor clientProcessor;

    @Mock
    private RevealJsonFormUtils jsonFormUtils;

    @Mock
    SQLiteDatabase database;

    @Captor
    ArgumentCaptor<BaseTaskDetails> baseTaskDetailsArgumentCaptor;

    private InteractorUtils interactorUtils;

    private String formSubmissionId;
    private String baseEntityId;

    @Before
    public void setUp() {
        interactorUtils = new InteractorUtils(taskRepository, eventClientRepository, clientProcessor);
        Whitebox.setInternalState(interactorUtils, "jsonFormUtils", jsonFormUtils);
        formSubmissionId = UUID.randomUUID().toString();
        baseEntityId = UUID.randomUUID().toString();
    }

    @Test
    public void testGetFormSubmissionIdsFromEventTask() {

        BaseTaskDetails taskDetails = new BaseTaskDetails("task-identifier");
        taskDetails.setTaskCode(BLOOD_SCREENING);
        taskDetails.setTaskEntity("entity-id");

        when(database.rawQuery(anyString(), any())).thenReturn(createEventCursor());

        List<String> actualFormSubmissionIds = interactorUtils.getFormSubmissionIdsFromEventTask(database, taskDetails);

        verify(database).rawQuery("select base_entity_id from event_task where task_id = ?", new String[]{taskDetails.getTaskId()});
        assertEquals(baseEntityId, actualFormSubmissionIds.get(0));
    }

    @Test
    public void testGetFormSubmissionIdsFromEventTaskForCaseConfirmation() {

        BaseTaskDetails taskDetails = new BaseTaskDetails("task-identifier");
        taskDetails.setTaskCode(CASE_CONFIRMATION);
        taskDetails.setTaskEntity("entity-id");

        when(database.rawQuery(anyString(), any())).thenReturn(createEventCursor());

        List<String> actualFormSubmissionIds = interactorUtils.getFormSubmissionIdsFromEventTask(database, taskDetails);

        verify(database).rawQuery("select formSubmissionId from event where baseEntityId = ?  and eventType = ?", new String[]{taskDetails.getTaskEntity(), CASE_CONFIRMATION_FIELD});
        assertEquals(formSubmissionId, actualFormSubmissionIds.get(0));
    }

    @Test
    public void testGetFormSubmissionIdsFromEventTaskForBCC() {

        BaseTaskDetails taskDetails = new BaseTaskDetails("task-identifier");
        taskDetails.setTaskCode(BCC);
        taskDetails.setTaskEntity("entity-id");

        when(database.rawQuery(anyString(), any())).thenReturn(createEventCursor());

        List<String> actualFormSubmissionIds = interactorUtils.getFormSubmissionIdsFromEventTask(database, taskDetails);

        verify(database).rawQuery("select formSubmissionId from event where baseEntityId = ?  and eventType = ?", new String[]{taskDetails.getTaskEntity(), BEHAVIOUR_CHANGE_COMMUNICATION});
        assertEquals(formSubmissionId, actualFormSubmissionIds.get(0));
    }

    @Ignore
    @Test
    public void testResetTaskInfo() {

        BaseTaskDetails taskDetails = new BaseTaskDetails("task-identifier");

        interactorUtils = spy(interactorUtils);
        interactorUtils.resetTaskInfo(RuntimeEnvironment.application, database, taskDetails);
        verify(interactorUtils).archiveEventsForTask(any(), baseTaskDetailsArgumentCaptor.capture());

    }

    private Cursor createEventCursor() {
        MatrixCursor cursor = new MatrixCursor(new String[]{FORM_SUBMISSION_ID, BASE_ENTITY_ID});
        cursor.addRow(new Object[]{formSubmissionId, baseEntityId});
        return cursor;
    }
}
