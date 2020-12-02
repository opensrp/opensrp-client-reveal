package org.smartregister.reveal.presenter;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.powermock.reflect.Whitebox;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.configurableviews.helper.ConfigurableViewsHelper;
import org.smartregister.configurableviews.model.View;
import org.smartregister.configurableviews.model.ViewConfiguration;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.EventRegisterContract;
import org.smartregister.reveal.interactor.EventRegisterFragmentInteractor;
import org.smartregister.reveal.util.Constants.EventsRegister;

import java.util.Collections;
import java.util.Set;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 12/1/20.
 */
public class EventRegisterFragmentPresenterTest extends BaseUnitTest {
    @Mock
    private EventRegisterContract.View view;

    @Mock
    private ConfigurableViewsHelper viewsHelper;

    @Mock
    private EventRegisterFragmentInteractor interactor;

    private EventRegisterFragmentPresenter presenter;

    private Set<View> visibleColumns;

    @Before
    public void setUp() {
        presenter = new EventRegisterFragmentPresenter(view, EventsRegister.VIEW_IDENTIFIER);
        ReflectionHelpers.setField(presenter, "viewsHelper", viewsHelper);
        ReflectionHelpers.setField(presenter, "interactor", interactor);
        View view = new View();
        view.setIdentifier(UUID.randomUUID().toString());
        visibleColumns = Collections.singleton(view);
    }

    @Test
    public void testProcessViewConfigurationsShouldRegisterActiveColumns() {
        ViewConfiguration viewConfiguration = new ViewConfiguration();
        viewConfiguration.setIdentifier(UUID.randomUUID().toString());
        when(viewsHelper.getViewConfiguration(EventsRegister.VIEW_IDENTIFIER)).thenReturn(viewConfiguration);
        presenter.processViewConfigurations();
        verify(viewsHelper).getViewConfiguration(eq(EventsRegister.VIEW_IDENTIFIER));
        verify(viewsHelper).getRegisterActiveColumns(eq(EventsRegister.VIEW_IDENTIFIER));
        verifyNoMoreInteractions(viewsHelper);
    }

    @Test
    public void testInitializeQueries() {
        String mainCondition = "task.group_id = ? AND task.plan_id = ? AND task.status NOT IN (?,?)";
        Whitebox.setInternalState(presenter, "visibleColumns", visibleColumns);
        presenter.initializeQueries(mainCondition);
        verify(view).initializeAdapter(eq(visibleColumns));
        String[] columns = ArrayUtils.addFirst(presenter.mainColumns(EventsRegister.TABLE_NAME), "ec_events.id as _id");
        verify(view).initializeQueryParams(EventsRegister.TABLE_NAME, "SELECT COUNT(*) FROM ec_events WHERE " + mainCondition + " ", String.format("Select %s FROM %s WHERE %s ", StringUtils.join(columns, " , "), EventsRegister.TABLE_NAME, mainCondition));
        verify(view).countExecute();
        verify(view).filterandSortInInitializeQueries();


    }
}
