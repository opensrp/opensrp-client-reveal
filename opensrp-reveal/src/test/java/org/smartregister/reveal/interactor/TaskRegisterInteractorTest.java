package org.smartregister.reveal.interactor;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.configurableviews.helper.ConfigurableViewsHelper;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.BaseContract;
import org.smartregister.reveal.util.Constants;

import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;

/**
 * Created by samuelgithengi on 3/27/19.
 */
public class TaskRegisterInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private BaseContract.BasePresenter presenter;

    @Mock
    private ConfigurableViewsHelper viewsHelper;

    private TaskRegisterInteractor interactor;

    @Before
    public void setUp() {
        interactor = new TaskRegisterInteractor(presenter);
        Whitebox.setInternalState(interactor, "viewsHelper", viewsHelper);
    }

    @Test
    public void testRegisterViewConfigurations() {
        List<String> identifiers = Collections.singletonList(Constants.TaskRegister.VIEW_IDENTIFIER);
        interactor.registerViewConfigurations(identifiers);
        verify(viewsHelper, timeout(ASYNC_TIMEOUT)).registerViewConfigurations(identifiers);
    }

    @Test
    public void testUnregisterViewConfiguration() {
        List<String> identifiers = Collections.singletonList(Constants.TaskRegister.VIEW_IDENTIFIER);
        interactor.unregisterViewConfiguration(identifiers);
        verify(viewsHelper, timeout(ASYNC_TIMEOUT)).unregisterViewConfiguration(identifiers);
    }

    @Test
    public void testCleanupResources() {
        interactor.cleanupResources();
        assertNull(Whitebox.getInternalState(interactor, "viewsHelper"));
        assertNull(Whitebox.getInternalState(interactor, "appExecutors"));
    }
}
