package org.smartregister.reveal.presenter;

import android.location.Location;
import android.support.v4.util.Pair;

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
import org.robolectric.RuntimeEnvironment;
import org.smartregister.Context;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.configurableviews.helper.ConfigurableViewsHelper;
import org.smartregister.configurableviews.model.View;
import org.smartregister.configurableviews.model.ViewConfiguration;
import org.smartregister.domain.Task;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.TaskRegisterFragmentContract;
import org.smartregister.reveal.interactor.TaskRegisterFragmentInteractor;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.TaskRegister;
import org.smartregister.reveal.util.LocationUtils;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.util.TestingUtils;
import org.smartregister.reveal.util.Utils;
import org.smartregister.util.Cache;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 3/27/19.
 */
public class TaskRegisterFragmentPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private TaskRegisterFragmentContract.View view;

    @Mock
    private TaskRegisterFragmentInteractor interactor;

    @Mock
    private LocationUtils locationUtils;

    @Mock
    private ConfigurableViewsHelper viewsHelper;

    @Mock
    private PreferencesUtil preferencesUtil;

    @Mock
    private RevealJsonFormUtils jsonFormUtils;

    @Captor
    private ArgumentCaptor<Pair<String, String[]>> mainConditionCaptor;

    @Captor
    private ArgumentCaptor<Location> myLocationCaptor;

    @Captor
    private ArgumentCaptor<Location> operationalAreaCenterCaptor;

    @Captor
    private ArgumentCaptor<String> labelCaptor;

    @Captor
    private ArgumentCaptor<List<TaskDetails>> taskDetailsArgumentCaptor;

    private TaskRegisterFragmentPresenter presenter;

    private Set<View> visibleColumns;

    private Location location;

    private org.smartregister.domain.Location operationalArea;
    private TaskDetails task1;
    private TaskDetails task2;
    private List<TaskDetails> taskList;

    @Before
    public void setUp() {
        Context.bindtypes = new ArrayList<>();
        when(view.getContext()).thenReturn(RuntimeEnvironment.application);
        presenter = new TaskRegisterFragmentPresenter(view, TaskRegister.VIEW_IDENTIFIER, interactor);
        Whitebox.setInternalState(presenter, "viewsHelper", viewsHelper);
        Whitebox.setInternalState(presenter, "prefsUtil", preferencesUtil);
        visibleColumns = new HashSet<>();
        View view = new View();
        view.setIdentifier(UUID.randomUUID().toString());
        visibleColumns.add(view);
        location = new Location("TaskRegisterFragmentPresenterTest");
        Cache<org.smartregister.domain.Location> cache = new Cache<>();
        operationalArea = TestingUtils.gson.fromJson(TestingUtils.operationalAreaGeoJSON, org.smartregister.domain.Location.class);
        cache.get("MTI_84", () -> operationalArea);
        Whitebox.setInternalState(Utils.class, cache);
        when(this.view.getLocationUtils()).thenReturn(locationUtils);
    }

    @Test
    public void testProcessViewConfigurations() {
        ViewConfiguration viewConfiguration = new ViewConfiguration();
        viewConfiguration.setIdentifier(UUID.randomUUID().toString());
        when(viewsHelper.getViewConfiguration(TaskRegister.VIEW_IDENTIFIER)).thenReturn(viewConfiguration);
        presenter.processViewConfigurations();
        verify(viewsHelper).getViewConfiguration(eq(TaskRegister.VIEW_IDENTIFIER));
        verify(viewsHelper).getRegisterActiveColumns(eq(TaskRegister.VIEW_IDENTIFIER));
        verifyNoMoreInteractions(viewsHelper);
    }

    @Test
    public void testInitializeQueries() {
        String mainCondition = "task.group_id = ? AND task.plan_id = ? AND task.status NOT IN (?,?)";
        Whitebox.setInternalState(presenter, "visibleColumns", visibleColumns);
        presenter.initializeQueries(mainCondition);
        verify(view).initializeAdapter(eq(visibleColumns));
        verify(view).showProgressView();
        verify(interactor).findTasks(mainConditionCaptor.capture(), myLocationCaptor.capture(), operationalAreaCenterCaptor.capture(), labelCaptor.capture());
        assertEquals(mainCondition, mainConditionCaptor.getValue().first);
        assertNull(mainConditionCaptor.getValue().second[0]);
        assertNull(mainConditionCaptor.getValue().second[1]);

        assertNull(myLocationCaptor.getValue());
        assertNull(operationalAreaCenterCaptor.getValue());
    }

    @Test
    public void testInitializeQueriesUsesCorrectParams() {
        String campaignId = UUID.randomUUID().toString();
        when(preferencesUtil.getCurrentPlanId()).thenReturn(campaignId);
        when(preferencesUtil.getCurrentOperationalArea()).thenReturn("MTI_84");
        String mainCondition = "task.group_id = ? AND task.plan_id = ? AND task.status NOT IN (?,?)";
        Whitebox.setInternalState(presenter, "visibleColumns", visibleColumns);
        when(locationUtils.getLastLocation()).thenReturn(location);
        presenter.initializeQueries(mainCondition);
        verify(view).initializeAdapter(eq(visibleColumns));
        verify(view).showProgressView();
        verify(interactor).findTasks(mainConditionCaptor.capture(), myLocationCaptor.capture(), operationalAreaCenterCaptor.capture(), labelCaptor.capture());
        assertEquals(mainCondition, mainConditionCaptor.getValue().first);
        assertEquals(operationalArea.getId(), mainConditionCaptor.getValue().second[0]);
        assertEquals(campaignId, mainConditionCaptor.getValue().second[1]);

        assertEquals(location, myLocationCaptor.getValue());

        assertEquals(-14.152197, operationalAreaCenterCaptor.getValue().getLatitude(), 0.0001);
        assertEquals(32.643570, operationalAreaCenterCaptor.getValue().getLongitude(), 0.0001);
    }

    @Test(expected = IllegalStateException.class)
    public void testStartSync() {
        presenter.startSync();
    }

    @Test
    public void testOnTasksFoundWithNullTasks() {
        presenter.onTasksFound(null, 0);
        verify(view).displayNotification(R.string.fetching_structure_title,
                R.string.fetch_location_and_structures_failed, preferencesUtil.getCurrentOperationalArea());
        verify(view).setTaskDetails(eq(new ArrayList<>()));
        verify(view).hideProgressDialog();
        verify(view).hideProgressView();
        verify(view).setTotalTasks(eq(0));
        verify(view).getContext();
        verifyNoMoreInteractions(view);

    }

    @Test
    public void testOnTasksFoundWithEmptyTasks() {
        presenter.onTasksFound(new ArrayList<>(), 0);
        verify(view).displayNotification(eq(R.string.fetching_structure_title), eq(R.string.no_structures_found));
        verify(view).setTaskDetails(eq(new ArrayList<>()));
        verify(view).hideProgressDialog();
        verify(view).hideProgressView();
        verify(view).setTotalTasks(eq(0));
        verify(view).getContext();
        verifyNoMoreInteractions(view);
    }

    @Test
    public void testOnTasksFoundWithTasks() {
        List<TaskDetails> detailsList = new ArrayList<>();
        detailsList.add(TestingUtils.getTaskDetails());
        presenter.onTasksFound(detailsList, 1);
        verify(view, never()).displayNotification(anyInt(), anyInt());
        verify(view).setTaskDetails(eq(detailsList));
        verify(view).hideProgressDialog();
        verify(view).hideProgressView();
        verify(view).setTotalTasks(eq(1));
        verify(view).getContext();
        verifyNoMoreInteractions(view);
    }

    @Test
    public void testOnTasksFoundRecalculatesDistance() {
        List<TaskDetails> detailsList = new ArrayList<>();
        detailsList.add(TestingUtils.getTaskDetails());
        Whitebox.setInternalState(presenter, "recalculateDistance", true);
        presenter.onTasksFound(detailsList, 1);
        verify(view).getContext();
        verifyNoMoreInteractions(view);
        assertFalse(Whitebox.getInternalState(presenter, "recalculateDistance"));
        verify(interactor).calculateDistanceFromUser(detailsList, null);
    }

    @Test
    public void testOnTasksWithFilterApplied() {
        List<TaskDetails> detailsList = new ArrayList<>();
        detailsList.add(TestingUtils.getTaskDetails());
        Whitebox.setInternalState(presenter, "applyFilterOnTasksFound", true);
        Whitebox.setInternalState(presenter, "filterParams", new TaskFilterParams("search"));
        presenter.onTasksFound(detailsList, 1);
        verify(view).setTotalTasks(1);
        verify(view).clearFilter();
        verify(view).setSearchPhrase("search");
        assertFalse(Whitebox.getInternalState(presenter, "applyFilterOnTasksFound"));
    }

    @Test
    public void testOnLocationChangedFirstTime() {
        presenter.onLocationChanged(location);
        assertEquals(location, Whitebox.getInternalState(presenter, "lastLocation"));
    }

    @Test
    public void testOnLocationChangedSubsequentTimeShouldCalculateDistance() throws Exception {
        presenter.onLocationChanged(location);
        when(preferencesUtil.getCurrentOperationalArea()).thenReturn("MTI_84");
        Location updateLocation = Whitebox.invokeMethod(presenter, "getOperationalAreaCenter");
        presenter.onLocationChanged(updateLocation);
        assertEquals(updateLocation, Whitebox.getInternalState(presenter, "lastLocation"));
        verify(interactor).calculateDistanceFromUser(null, updateLocation);
    }

    @Test
    public void testOnDestroy() {
        presenter.onDestroy();
        verify(locationUtils).stopLocationClient();
    }

    @Test
    public void testOnDrawerClosed() {
        String campaignId = "IRS_2019_season1";
        PreferencesUtil.getInstance().setCurrentPlanId(campaignId);
        when(preferencesUtil.getCurrentPlanId()).thenReturn(campaignId);
        when(preferencesUtil.getCurrentOperationalArea()).thenReturn("MTI_84");
        presenter.onLocationChanged(location);
        presenter.onDrawerClosed();
        verify(view).showProgressDialog(R.string.fetching_structures_title, R.string.fetching_structures_message);
        verify(interactor).findTasks(mainConditionCaptor.capture(), myLocationCaptor.capture(), operationalAreaCenterCaptor.capture(), labelCaptor.capture());
        assertEquals("task.group_id = ? AND task.plan_id = ? AND task.status NOT IN (?,?)", mainConditionCaptor.getValue().first);
        assertEquals(operationalArea.getId(), mainConditionCaptor.getValue().second[0]);
        assertEquals(campaignId, mainConditionCaptor.getValue().second[1]);

        assertEquals(location, myLocationCaptor.getValue());

        assertEquals(-14.152197, operationalAreaCenterCaptor.getValue().getLatitude(), 0.0001);
        assertEquals(32.643570, operationalAreaCenterCaptor.getValue().getLongitude(), 0.0001);
    }

    @Test
    public void testOnTaskSelectedForCompletedTasks() {
        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        presenter.onTaskSelected(taskDetails, true);
        verify(view).showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
        verify(interactor).getStructure(taskDetails);
    }

    @Test
    public void testOnTaskSelectedShouldLaunchForm() {
        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        taskDetails.setTaskStatus(Task.TaskStatus.READY.name());
        presenter.onTaskSelected(taskDetails, false);
        verify(view).showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
        verify(interactor).getStructure(taskDetails);
        verify(view).getContext();
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);
    }

    @Test
    public void testOnTaskSelectedForBCCTasks() {
        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        taskDetails.setTaskCode(Constants.Intervention.BCC);
        presenter.onTaskSelected(taskDetails, true);
        verify(view).getContext();
        verify(view).showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
        verify(interactor).getStructure(taskDetails);
    }

    @Test
    public void testOnStructureFound() {
        when(view.getJsonFormUtils()).thenReturn(mock(RevealJsonFormUtils.class));
        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        presenter.onStructureFound(null, taskDetails);
        verify(view).getContext();
        verify(view).requestUserLocation();
        verify(view).getUserCurrentLocation();
        verifyNoMoreInteractions(view);
    }

    @Test
    public void testOnStructureFoundWithLocationValidationDisabled() {
        when(view.getJsonFormUtils()).thenReturn(jsonFormUtils);
        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        when(jsonFormUtils.getFormName(null, taskDetails.getTaskCode())).thenReturn(Constants.JsonForm.SPRAY_FORM);
        presenter = spy(presenter);
        doReturn(false).when(presenter).validateFarStructures();
        presenter.onStructureFound(new org.smartregister.domain.Location(), taskDetails);
        verify(view, timeout(ASYNC_TIMEOUT)).startForm(any());
        verify(view).hideProgressDialog();
    }

    @Test
    public void testSearchGloballyDoesNothing() {
        presenter.searchGlobally("");
        verify(view).getContext();
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);

    }

    @Test
    public void testOnFormSaved() {
        presenter.onFormSaved(null, null, null, null, null);
        verify(view).getContext();
        verify(view).hideProgressDialog();
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);

    }


    @Test
    public void testOnStructureAdded() {
        presenter.onStructureAdded(null, null, 17);
        verify(view).getContext();
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);

    }


    @Test
    public void testOnFormSaveFailure() {
        presenter.onFormSaveFailure(null);
        verify(view).getContext();
        verify(view).hideProgressDialog();
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);

    }

    @Test
    public void testOnFamilyNotFound() {
        presenter.onFamilyFound(null);
        verify(view).getContext();
        verify(view).displayNotification(R.string.fetch_family_failed, R.string.failed_to_find_family);
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);

    }

    @Test
    public void testOnFamilyFound() {
        CommonPersonObjectClient family = new CommonPersonObjectClient(UUID.randomUUID().toString(), null, null);
        presenter.onFamilyFound(family);
        verify(view).getContext();
        verify(view).openFamilyProfile(family, null);
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);

    }

    @Test
    public void testOnTaskSelectedFetchesCaseDetails() {
        String eventId = UUID.randomUUID().toString();
        when(preferencesUtil.getCurrentOperationalArea()).thenReturn("MTI_84");

        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        taskDetails.setTaskCode(Constants.Intervention.CASE_CONFIRMATION);
        taskDetails.setReasonReference(eventId);
        presenter.onTaskSelected(taskDetails, true);
        verify(view).getContext();
        verify(interactor).getIndexCaseDetails(taskDetails.getStructureId(), operationalArea.getId(), eventId);
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);
    }

    @Test
    public void testOnTaskSelectedOpensFamilyProfile() {
        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        taskDetails.setTaskCode(Constants.Intervention.BLOOD_SCREENING);
        taskDetails.setTaskCount(2);
        taskDetails.setStructureId(UUID.randomUUID().toString());
        presenter.onTaskSelected(taskDetails, true);
        verify(view).getContext();
        verify(interactor).fetchFamilyDetails(taskDetails.getStructureId());
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);
    }

    @Test
    public void testOnLocationValidatedStartsFamilyForm() {
        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        taskDetails.setTaskCode(Constants.Intervention.REGISTER_FAMILY);
        presenter.setTaskDetails(taskDetails);
        presenter.onLocationValidated();
        verify(view).registerFamily(taskDetails);
        verify(view).hideProgressDialog();
        verifyNoMoreInteractions(interactor);
    }

    @Test
    public void testOnIndexCaseNotFound() {
        presenter.onIndexCaseFound(null, false);
        verify(view).displayError(R.string.classification_details, R.string.index_case_not_found);
        verify(view).getContext();
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);
    }

    @Test
    public void testOnIndexLinkedToJurisdictionCaseFound() {
        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        taskDetails.setTaskCode(Constants.Intervention.CASE_CONFIRMATION);
        presenter.setTaskDetails(taskDetails);
        JSONObject indexCase = new JSONObject();
        presenter.onIndexCaseFound(indexCase, true);
        verify(view).displayIndexCaseDetails(indexCase);
        verify(view).getContext();
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);
    }

    @Test
    public void testOnIndexLinkedToStructureCaseAndNotCompletedFound() {
        TaskDetails taskDetails = TestingUtils.getTaskDetails();
        taskDetails.setTaskCode(Constants.Intervention.CASE_CONFIRMATION);
        taskDetails.setTaskStatus(Task.TaskStatus.READY.name());
        presenter.setTaskDetails(taskDetails);
        JSONObject indexCase = new JSONObject();
        Whitebox.setInternalState(presenter, "isActionClicked", true);
        presenter.onIndexCaseFound(indexCase, false);
        verify(interactor).fetchFamilyDetails(taskDetails.getStructureId());
        verify(view).getContext();
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);
    }

    private void initSearchTasks() {
        task1 = TestingUtils.getTaskDetails();
        task2 = new TaskDetails("task2");
        task2.setFamilyMemberNames("Jane Doe,John Doe,Kenny Rodger");
        task2.setDistanceFromUser(24f);
        taskList = Arrays.asList(task1, task2);
        Whitebox.setInternalState(presenter, "tasks", taskList);
    }

    @Test
    public void testSearchTasks() {
        initSearchTasks();
        presenter.searchTasks("Kenny");
        verify(view).setTaskDetails(taskList);
        verify(view).setTotalTasks(1);


    }


    @Test
    public void testSearchTaskFamilyMembers() {
        initSearchTasks();
        presenter.searchTasks("Jane Doe");
        verify(view).setTaskDetails(taskDetailsArgumentCaptor.capture());
        verify(view).setTotalTasks(1);

        assertEquals(1, taskDetailsArgumentCaptor.getValue().size());
        assertEquals(task2.getTaskId(), taskDetailsArgumentCaptor.getValue().get(0).getTaskId());

    }


    @Test
    public void testSearchTasksByStructure() {
        initSearchTasks();
        presenter.searchTasks("Kenny House");
        verify(view).setTaskDetails(taskDetailsArgumentCaptor.capture());
        verify(view).setTotalTasks(0);

        assertEquals(1, taskDetailsArgumentCaptor.getValue().size());
        assertEquals(task1.getTaskId(), taskDetailsArgumentCaptor.getValue().get(0).getTaskId());

    }

    @Test
    public void testSearchTasksByMissingPhrase() {
        initSearchTasks();
        presenter.searchTasks("Pluto");
        verify(view).setTaskDetails(taskDetailsArgumentCaptor.capture());
        verify(view).setTotalTasks(0);

        assertEquals(0, taskDetailsArgumentCaptor.getValue().size());


    }


}
