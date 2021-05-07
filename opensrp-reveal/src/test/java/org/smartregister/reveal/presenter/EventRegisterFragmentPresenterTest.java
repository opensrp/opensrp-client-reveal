package org.smartregister.reveal.presenter;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.joda.time.LocalDate;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.Mock;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.configurableviews.helper.ConfigurableViewsHelper;
import org.smartregister.configurableviews.model.View;
import org.smartregister.configurableviews.model.ViewConfiguration;
import org.smartregister.domain.Event;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.EventRegisterContract;
import org.smartregister.reveal.interactor.EventRegisterFragmentInteractor;
import org.smartregister.reveal.model.EventRegisterDetails;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.EventsRegister;
import org.smartregister.reveal.util.RevealJsonFormUtils;

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.endsWith;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 12/1/20.
 */
public class EventRegisterFragmentPresenterTest extends BaseUnitTest {
    private final String EMPTY = "";

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
    public void testOnEventFoundNoFormDisplaysError() {
        RevealJsonFormUtils revealJsonFormUtils = mock(RevealJsonFormUtils.class);
        when(view.getJsonFormUtils()).thenReturn(revealJsonFormUtils);
        when(revealJsonFormUtils.getFormName(anyString())).thenReturn(EMPTY);
        presenter.setEventRegisterDetails(mock(EventRegisterDetails.class));
        presenter.onEventFound(mock(Event.class));
        verify(view, times(1)).displayError(eq(R.string.opening_form_title), eq(R.string.form_not_found));
        verify(view, times(1)).hideProgressDialog();
    }

    private EventRegisterDetails sprayEventRegisterDetails() {
        EventRegisterDetails eventRegisterDetails = mock(EventRegisterDetails.class);
        when(eventRegisterDetails.getEventType()).thenReturn(Constants.EventType.SPRAY_EVENT);
        return eventRegisterDetails;
    }

    @SuppressWarnings("Convert2Lambda")
    @Test
    public void testOnEventFoundPopulatesForm() {
        presenter.setEventRegisterDetails(sprayEventRegisterDetails());
        RevealJsonFormUtils revealJsonFormUtils = spy(RevealJsonFormUtils.class);
        when(view.getJsonFormUtils()).thenReturn(revealJsonFormUtils);
        when(view.getContext()).thenReturn(RuntimeEnvironment.application);
        Event event = mock(Event.class);
        when(event.getBaseEntityId()).thenReturn("168783");
        when(event.getDetails()).thenReturn(Collections.singletonMap("taskIdentifier", "testing"));

        presenter.onEventFound(event);

        verify(revealJsonFormUtils).getFormJSON(eq(view.getContext()), anyString(), isNull(), isNull());
        verify(revealJsonFormUtils).populateForm(eq(event), argThat(new ArgumentMatcher<JSONObject>() {
            @Override
            public boolean matches(JSONObject argument) {
                try {
                    return argument.has("count") && argument.has("step1") && argument.getJSONObject("step1").has("fields");
                } catch (JSONException e) {
                    e.printStackTrace();
                    return false;
                }
            }
        }));
        verify(revealJsonFormUtils).populateFormWithServerOptions(anyString(), any(JSONObject.class));
        verify(view).startForm(argThat(new ArgumentMatcher<JSONObject>() {
            @Override
            public boolean matches(JSONObject argument) {
                try {
                    return (argument.has(Constants.ENTITY_ID) && argument.has(Constants.DETAILS)
                            && argument.getString(Constants.ENTITY_ID).equals(event.getBaseEntityId())
                            && argument.getJSONObject(Constants.DETAILS).toString().equals(new JSONObject(event.getDetails()).toString()));
                } catch (JSONException e) {
                    e.printStackTrace();
                    return false;
                }
            }
        }));
    }

    @Test
    public void testOnOpenMapClickedStartsMap() {
        presenter.onOpenMapClicked();
        verify(view, times(1)).startMapActivity();
    }

    @Test
    public void testOnEventSelectedShowsProgressDialog() {
        EventRegisterDetails eventRegisterDetails = mock(EventRegisterDetails.class);
        presenter.onEventSelected(eventRegisterDetails);
        verify(view, times(1)).showProgressDialog(eq(R.string.opening_form_title), eq(R.string.opening_form_message));
    }

    @Test
    public void testOnEventSelectedUpdatesPresenterEventRegisterDetails() {
        EventRegisterDetails eventRegisterDetails = mock(EventRegisterDetails.class);
        presenter.onEventSelected(eventRegisterDetails);
        assertThat(presenter.getEventRegisterDetails(), is(equalTo(eventRegisterDetails)));
    }

    @Test
    public void testOnEventSelectedFindsEvent() {
        EventRegisterDetails eventRegisterDetails = mock(EventRegisterDetails.class);
        presenter.onEventSelected(eventRegisterDetails);
        verify(interactor, times(1)).findEvent(eq(eventRegisterDetails.getFormSubmissionId()));
    }

    @Test
    public void testOnFilterTasksClickedOpensActivity() {
        presenter.onFilterTasksClicked();
        verify(view).openFilterActivity(any());
    }

    @Test
    public void testFilterTasksUpdatesFilterParams() {
        TaskFilterParams taskFilterParams = mock(TaskFilterParams.class);
        presenter.filterTasks(taskFilterParams);
        assertThat(presenter.getFilterParams(), is(equalTo(taskFilterParams)));
    }

    @Test
    public void testFilterTasksFiltersOnView() {
        TaskFilterParams taskFilterParams = mock(TaskFilterParams.class);
        presenter.filterTasks(taskFilterParams);
        verify(view, times(1)).filterandSortInInitializeQueries();
    }

    @Test
    public void testGetMainConditionWhenFilterParamNull() {
        String mainCondition = presenter.getMainCondition();
        assertNull(presenter.getFilterParams());
        assertThat(mainCondition, containsString(String.format("%s = ", Constants.DatabaseKeys.PROVIDER_ID)));
    }

    @Test
    public void testGetMainConditionWhenFilterParamNotIsViewAllEvents() {
        TaskFilterParams taskFilterParams = mock(TaskFilterParams.class);
        when(taskFilterParams.isViewAllEvents()).thenReturn(false);
        presenter.setFilterParams(taskFilterParams);
        String mainCondition = presenter.getMainCondition();
        assertThat(mainCondition, containsString(String.format("%s = ", Constants.DatabaseKeys.PROVIDER_ID)));
    }

    @Test
    public void testGetMainConditionWhenFilterParamsHasForm() {
        TaskFilterParams taskFilterParams = mock(TaskFilterParams.class);
        Map<String, Set<String>> filterWithForm = new HashMap<>(1);
        filterWithForm.put(Constants.Filter.FORM_NAME, Collections.singleton("random_form"));
        when(taskFilterParams.getCheckedFilters()).thenReturn(filterWithForm);
        presenter.setFilterParams(taskFilterParams);
        String mainConditionResult = presenter.getMainCondition();
        assertThat(mainConditionResult, containsString(String.format("%s IN ", Constants.DatabaseKeys.EVENT_TYPE)));
    }

    @Test
    public void testGetMainConditionWhenFilterParamsHasStatus() {
        TaskFilterParams taskFilterParams = mock(TaskFilterParams.class);
        Map<String, Set<String>> filterWithStatus = new HashMap<>(1);
        filterWithStatus.put(Constants.Filter.STATUS, new HashSet<>(Collections.singletonList("random status")));
        when(taskFilterParams.getCheckedFilters()).thenReturn(filterWithStatus);
        presenter.setFilterParams(taskFilterParams);
        String mainConditionResult = presenter.getMainCondition();
        assertThat(mainConditionResult, containsString(String.format("%s IN ", Constants.DatabaseKeys.STATUS)));
    }

    @Test
    public void testGetMainConditionWhenFilterParamsHasFromDate() {
        TaskFilterParams taskFilterParams = mock(TaskFilterParams.class);
        Date today = new Date();
        when(taskFilterParams.getFromDate()).thenReturn(today);
        presenter.setFilterParams(taskFilterParams);
        String mainConditionResult = presenter.getMainCondition();
        assertThat(mainConditionResult, containsString(Constants.DatabaseKeys.EVENT_DATE));
        assertThat(mainConditionResult, containsString(new LocalDate(today.getTime()).toString()));
    }

    @Test
    public void testGetSortQueryDefaultsToByDate() {
        String sortQueryResult = presenter.getSortQuery();
        assertThat(sortQueryResult, is(equalTo(String.format("%s DESC", Constants.DatabaseKeys.EVENT_DATE))));
    }

    @Test
    public void testSortQueryByForm() {
        TaskFilterParams taskFilterParams = mock(TaskFilterParams.class);
        when(view.getContext()).thenReturn(RuntimeEnvironment.application);
        when(taskFilterParams.getSortBy()).thenReturn("Form Name");
        presenter.setFilterParams(taskFilterParams);
        String sortQueryResult = presenter.getSortQuery();
        assertThat(sortQueryResult, endsWith(Constants.DatabaseKeys.EVENT_TYPE));
    }

    @Test
    public void testSortQueryBySOP() {
        TaskFilterParams taskFilterParams = mock(TaskFilterParams.class);
        when(view.getContext()).thenReturn(RuntimeEnvironment.application);
        when(taskFilterParams.getSortBy()).thenReturn("SOP");
        presenter.setFilterParams(taskFilterParams);
        String sortQueryResult = presenter.getSortQuery();
        assertThat(sortQueryResult, endsWith(Constants.DatabaseKeys.SOP));
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
