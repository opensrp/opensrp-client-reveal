package org.smartregister.reveal.interactor;

import android.content.Context;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.sync.ClientProcessorForJava;

import java.util.UUID;
import java.util.concurrent.Executors;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;

/**
 * Created by samuelgithengi on 4/25/19.
 */
public class RevealFamilyProfileInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private FamilyProfileContract.Presenter presenter;

    @Mock
    private TaskUtils taskUtils;

    private RevealFamilyProfileInteractor interactor;

    private Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        interactor = new RevealFamilyProfileInteractor(presenter);
        Whitebox.setInternalState(interactor, "taskUtils", taskUtils);
        AppExecutors appExecutors = new AppExecutors(Executors.newSingleThreadExecutor(),
                Executors.newSingleThreadExecutor(), Executors.newSingleThreadExecutor());
        Whitebox.setInternalState(interactor, "appExecutors", appExecutors);
    }

    @Test
    public void testGetClientProcessorForJava() {
        ClientProcessorForJava clientProcessor = interactor.getClientProcessorForJava();
        assertNotNull(clientProcessor);
        assertTrue(clientProcessor instanceof RevealClientProcessor);
    }

    @Test
    public void testGenerateTasks() {
        String baseEntityId = UUID.randomUUID().toString();
        interactor.generateTasks(context, baseEntityId);
        verify(taskUtils, timeout(ASYNC_TIMEOUT)).generateBloodScreeningTask(context, baseEntityId);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onTasksGenerated();
    }


    @Test
    public void testUpdateFamilyMemberSurname() {

    }

}
