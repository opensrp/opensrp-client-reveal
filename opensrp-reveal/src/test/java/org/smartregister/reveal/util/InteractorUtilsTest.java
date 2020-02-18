package org.smartregister.reveal.util;

import net.sqlcipher.Cursor;
import net.sqlcipher.MatrixCursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.domain.db.Event;
import org.smartregister.domain.db.EventClient;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.model.BaseTaskDetails;
import org.smartregister.reveal.sync.RevealClientProcessor;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
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
    ArgumentCaptor<JSONArray> jsonArrayArgumentCaptor;

    @Captor
    ArgumentCaptor<Long> longArgumentCaptor;

    @Captor
    ArgumentCaptor<String> stringArgumentCaptor;

    @Captor
    ArgumentCaptor<JSONObject> jsonObjectArgumentCaptor;

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

    @Test
    public void testArchiveEventsForTask() throws Exception {
        BaseTaskDetails taskDetails = TestingUtils.getTaskDetails();
        taskDetails.setTaskCode(BLOOD_SCREENING);

        Event event = new Event();
        event.setBaseEntityId(baseEntityId);
        EventClient eventClient = new EventClient(event);
        List<EventClient> eventClientList = new ArrayList<>();
        eventClientList.add(eventClient);

        when(database.rawQuery(anyString(), any())).thenReturn(createEventCursor());
        when(eventClientRepository.fetchEventClients(any())).thenReturn(eventClientList);

        interactorUtils.archiveEventsForTask(database, taskDetails);

        verify(eventClientRepository).batchInsertEvents(jsonArrayArgumentCaptor.capture(), longArgumentCaptor.capture());

        JSONObject actualEvent = jsonArrayArgumentCaptor.getAllValues().get(0).getJSONObject(0);

        assertEquals(0, longArgumentCaptor.getValue().intValue());
        assertNotNull(actualEvent.get("dateVoided"));
        assertEquals(BaseRepository.TYPE_Unsynced, actualEvent.get(EventClientRepository.event_column.syncStatus.name()));

        verify(eventClientRepository).addEvent(stringArgumentCaptor.capture(), jsonObjectArgumentCaptor.capture());

        assertEquals(taskDetails.getTaskEntity(), stringArgumentCaptor.getValue());

        //JSONObject actualResetTaskEvent = jsonObjectArgumentCaptor.getValue();
        //assertEquals(BaseRepository.TYPE_Unsynced, actualResetTaskEvent.get(EventClientRepository.event_column.syncStatus.name()));


    }


    private Cursor createEventCursor() {
        MatrixCursor cursor = new MatrixCursor(new String[]{FORM_SUBMISSION_ID, BASE_ENTITY_ID});
        cursor.addRow(new Object[]{formSubmissionId, baseEntityId});
        return cursor;
    }
}
