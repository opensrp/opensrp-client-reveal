package org.smartregister.reveal.util;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.util.Cache;

import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.powermock.api.mockito.PowerMockito.when;
import static org.smartregister.domain.Task.TaskStatus.READY;
import static org.smartregister.repository.BaseRepository.TYPE_Unsynced;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;

/**
 * Created by Richard Kareko on 2/17/20.
 */

public class TaskUtilsTests extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private TaskRepository taskRepository;

    @Mock
    private PreferencesUtil prefsUtil;

    @Captor
    private ArgumentCaptor<Task> taskArgumentCaptor;

    private TaskUtils taskUtils;

    @Before
    public void setUp() {
        taskUtils = TaskUtils.getInstance();
        Whitebox.setInternalState(taskUtils, "taskRepository", taskRepository);
    }

    @Test
    public void testResetTask() {
        String planId = UUID.randomUUID().toString();
        String jurisdictionId = UUID.randomUUID().toString();
        String taskIdentifier = UUID.randomUUID().toString();
        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        taskDetails.setTaskCode(CASE_CONFIRMATION);

        Task task = TestingUtils.getTask("task_1");
        task.setCode(CASE_CONFIRMATION);
        task.setIdentifier(taskIdentifier);



        when(prefsUtil.getCurrentPlanId()).thenReturn(planId);
        when(prefsUtil.getCurrentOperationalArea()).thenReturn(jurisdictionId);
        Location jurisdiction = new Location();
        jurisdiction.setId(jurisdictionId);
        Cache<Location> cache = mock(Cache.class);
        when(cache.get(anyString(), any())).thenReturn(jurisdiction);
        Whitebox.setInternalState(Utils.class, cache);

        when(taskRepository.getTaskByIdentifier(anyString())).thenReturn(task);

        taskUtils.resetTask(RuntimeEnvironment.application, taskDetails);

        verify(taskRepository).addOrUpdate(taskArgumentCaptor.capture());
        assertEquals(task.getIdentifier(), taskArgumentCaptor.getValue().getIdentifier());
        assertEquals(jurisdictionId, taskArgumentCaptor.getValue().getForEntity());
        assertEquals(NOT_VISITED, taskArgumentCaptor.getValue().getBusinessStatus());
        assertEquals(READY, taskArgumentCaptor.getValue().getStatus());
        assertEquals(TYPE_Unsynced, taskArgumentCaptor.getValue().getSyncStatus());


    }
}
