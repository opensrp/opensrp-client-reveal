package org.smartregister.reveal.sync;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.Event;
import org.smartregister.domain.db.EventClient;
import org.smartregister.domain.jsonmapping.ClientClassification;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.spy;
import static org.powermock.api.mockito.PowerMockito.verifyPrivate;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.MOSQUITO_COLLECTION_EVENT;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;

/**
 * @author Vincent Karuri
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({RevealClientProcessor.class, Utils.class, PreferencesUtil.class})
public class RevealClientProcessorPowerMockTest {

    private RevealClientProcessor clientProcessor;

    @Before
    public void setUp() {
       clientProcessor = spy(Whitebox.newInstance(RevealClientProcessor.class));
    }

    @Test
    public void testProcessClientShouldCallProcessSprayEventWhenIsSprayEventType() throws Exception {
        mockStaticMethods();

        List<EventClient> eventClients = new ArrayList<>();
        Event event = new Event();
        event.setEventType(SPRAY_EVENT);

        EventClient eventClient = new EventClient(event, null);
        eventClients.add(eventClient);


        PowerMockito.doReturn(new ClientClassification()).when(clientProcessor, "assetJsonToJava", anyString(), any());
        PowerMockito.doReturn("").when(clientProcessor, "processSprayEvent", any(Event.class), any(ClientClassification.class), anyBoolean());
        clientProcessor.processClient(eventClients, true);

        PowerMockito.verifyPrivate(clientProcessor, times(1)).invoke("processSprayEvent", eq(event), any(ClientClassification.class), anyBoolean());
    }

    @Test
    public void testProcessClientShouldCallProcessMosquitoCollectionEventWhenIsMosquitoCollectionEventType() throws Exception {
        mockStaticMethods();
        List<EventClient> eventClients = new ArrayList<>();
        Event event = new Event();
        event.setEventType(MOSQUITO_COLLECTION_EVENT);

        EventClient eventClient = new EventClient(event, null);
        eventClients.add(eventClient);

        PowerMockito.doReturn(new ClientClassification()).when(clientProcessor, "assetJsonToJava", anyString(), any());
        PowerMockito.doReturn("").when(clientProcessor, "processMosquitoCollectionEvent", any(Event.class), any(ClientClassification.class), anyBoolean());
        clientProcessor.processClient(eventClients, true);

        PowerMockito.verifyPrivate(clientProcessor, times(1)).invoke("processMosquitoCollectionEvent", eq(event), any(ClientClassification.class), anyBoolean());
    }

    @Test
    public void testUpdateTaskShouldUpdateTask() throws Exception {
        Event event = mock(Event.class);

        Map<String, String> details = new HashMap<>();
        details.put(TASK_IDENTIFIER, "");
        doReturn(details).when(event).getDetails();

        TaskRepository taskRepository = mock(TaskRepository.class);
        Task task = new Task();
        Whitebox.setInternalState(clientProcessor, "taskRepository", taskRepository);
        doReturn(task).when(taskRepository).getTaskByIdentifier(anyString());
        doNothing().when(taskRepository).addOrUpdate(any(Task.class));

        Whitebox.invokeMethod(clientProcessor, "updateTask", event, true, IRS);

        verify(taskRepository, times(1)).addOrUpdate(eq(task));
        assertEquals(task.getStatus(), Task.TaskStatus.COMPLETED);
    }

    @Test
    public void testUpdateTaskShouldMarkEventAsTaskUnprocessed() throws Exception {
        Event event = mock(Event.class);

        Map<String, String> details = new HashMap<>();
        details.put(TASK_IDENTIFIER, "");
        doReturn(details).when(event).getDetails();

        final String FORM_SUBMISSION_ID = "form_submission_id";
        doReturn(FORM_SUBMISSION_ID).when(event).getFormSubmissionId();

        TaskRepository taskRepository = mock(TaskRepository.class);
        Whitebox.setInternalState(clientProcessor, "taskRepository", taskRepository);
        doReturn(null).when(taskRepository).getTaskByIdentifier(anyString());

        EventClientRepository eventClientRepository = mock(EventClientRepository.class);
        Whitebox.setInternalState(clientProcessor, "eventClientRepository", eventClientRepository);
        doNothing().when(eventClientRepository).markEventAsTaskUnprocessed(anyString());

        Whitebox.invokeMethod(clientProcessor, "updateTask", event, true, IRS);

        verify(eventClientRepository).markEventAsTaskUnprocessed(eq(FORM_SUBMISSION_ID));
    }

    @Test
    public void testUpdateTaskShouldMarkEventAsSynced() throws Exception {
        Event event = mock(Event.class);

        Map<String, String> details = new HashMap<>();
        details.put(TASK_IDENTIFIER, "");
        doReturn(details).when(event).getDetails();

        final String FORM_SUBMISSION_ID = "form_submission_id";
        doReturn(FORM_SUBMISSION_ID).when(event).getFormSubmissionId();

        TaskRepository taskRepository = mock(TaskRepository.class);
        Task task = new Task();
        Whitebox.setInternalState(clientProcessor, "taskRepository", taskRepository);
        doReturn(task).when(taskRepository).getTaskByIdentifier(anyString());
        doNothing().when(taskRepository).addOrUpdate(any(Task.class));

        EventClientRepository eventClientRepository = mock(EventClientRepository.class);
        Whitebox.setInternalState(clientProcessor, "eventClientRepository", eventClientRepository);
        doNothing().when(eventClientRepository).markEventAsSynced(anyString());

        Whitebox.invokeMethod(clientProcessor, "updateTask", event, false, IRS);

        verify(eventClientRepository).markEventAsSynced(eq(FORM_SUBMISSION_ID));
    }

    @Test
    public void testUpdateTaskShouldSetTaskSyncStatusToUnsynced() throws Exception {
        Event event = mock(Event.class);

        Map<String, String> details = new HashMap<>();
        details.put(TASK_IDENTIFIER, "");
        doReturn(details).when(event).getDetails();

        final String FORM_SUBMISSION_ID = "form_submission_id";
        doReturn(FORM_SUBMISSION_ID).when(event).getFormSubmissionId();

        TaskRepository taskRepository = mock(TaskRepository.class);
        Task task = new Task();
        task.setSyncStatus(BaseRepository.TYPE_Synced);
        Whitebox.setInternalState(clientProcessor, "taskRepository", taskRepository);
        doReturn(task).when(taskRepository).getTaskByIdentifier(anyString());
        doNothing().when(taskRepository).addOrUpdate(any(Task.class));

        EventClientRepository eventClientRepository = mock(EventClientRepository.class);
        Whitebox.setInternalState(clientProcessor, "eventClientRepository", eventClientRepository);
        doNothing().when(eventClientRepository).markEventAsSynced(anyString());

        Whitebox.invokeMethod(clientProcessor, "updateTask", event, true, IRS);

        assertEquals(task.getSyncStatus(), BaseRepository.TYPE_Unsynced);
    }

    private void mockStaticMethods() {
        mockStatic(Utils.class);
        mockStatic(PreferencesUtil.class);

        PreferencesUtil preferencesUtil = mock(PreferencesUtil.class);
        PowerMockito.when(preferencesUtil.getCurrentOperationalArea()).thenReturn("");

        PowerMockito.when(PreferencesUtil.getInstance()).thenReturn(preferencesUtil);
        PowerMockito.when(Utils.getOperationalAreaLocation(anyString())).thenReturn(new Location());
    }
}
