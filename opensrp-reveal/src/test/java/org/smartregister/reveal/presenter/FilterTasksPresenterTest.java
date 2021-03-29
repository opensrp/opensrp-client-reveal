package org.smartregister.reveal.presenter;

import android.content.Intent;
import android.widget.ToggleButton;

import com.google.android.flexbox.FlexboxLayout;

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
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FilterTasksContract;
import org.smartregister.reveal.model.FilterConfiguration;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.Filter;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Constants.InterventionType;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.TestingUtils;

import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 1/28/20.
 */
public class FilterTasksPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private FilterTasksContract.View view;

    @Mock
    private FlexboxLayout flexboxLayout;

    @Mock
    private ToggleButton toggleButton;

    @Captor
    private ArgumentCaptor<Intent> intentArgumentCaptor;

    @Captor
    private ArgumentCaptor<Date> dateArgumentCaptor;

    private FilterTasksPresenter filterTasksPresenter;

    private String planId = UUID.randomUUID().toString();

    private FilterConfiguration filterConfiguration = FilterConfiguration.builder().build();

    @Before
    public void setUp() {
        filterTasksPresenter = new FilterTasksPresenter(view, filterConfiguration);
        PreferencesUtil.getInstance().setCurrentPlan(planId);
    }

    @Test
    public void testPopulateLabels() {
        Map<String, Integer> labelsMap = Whitebox.getInternalState(filterTasksPresenter, "labelsMap");
        assertEquals(36, labelsMap.size());
        assertEquals(R.string.irs, labelsMap.get(Intervention.IRS).intValue());
        assertEquals(R.string.in_progress, labelsMap.get(BusinessStatus.IN_PROGRESS).intValue());
    }

    @Test
    public void testGetStringResource() {
        assertEquals(R.string.irs, filterTasksPresenter.getStringResource(Intervention.IRS).intValue());
        assertEquals(R.string.in_progress, filterTasksPresenter.getStringResource(BusinessStatus.IN_PROGRESS).intValue());
        assertEquals(R.string.operational_area, filterTasksPresenter.getStringResource(InterventionType.OPERATIONAL_AREA).intValue());
        assertNull(filterTasksPresenter.getStringResource("anu12"));
    }

    @Test
    public void testOnToggleChanged() {
        filterTasksPresenter.onToggleChanged(true, Filter.STATUS, BusinessStatus.BLOOD_SCREENING_COMPLETE);
        verify(view).onFiltedSelected(1);
        Map<String, Set<String>> filters = Whitebox.getInternalState(filterTasksPresenter, "checkedFilters");
        assertEquals(1, filters.size());
        assertEquals(1, filters.get(Filter.STATUS).size());
        assertEquals(BusinessStatus.BLOOD_SCREENING_COMPLETE, filters.get(Filter.STATUS).iterator().next());

        filterTasksPresenter.onToggleChanged(false, Filter.STATUS, BusinessStatus.BLOOD_SCREENING_COMPLETE);
        verify(view).onFiltedSelected(0);
        filters = Whitebox.getInternalState(filterTasksPresenter, "checkedFilters");
        assertTrue(filters.isEmpty());

    }

    @Test
    public void testOnToggleChangedMultipleFilters() {
        filterTasksPresenter.onToggleChanged(true, Filter.STATUS, BusinessStatus.BLOOD_SCREENING_COMPLETE);
        filterTasksPresenter.onToggleChanged(true, Filter.STATUS, BusinessStatus.COMPLETE);
        filterTasksPresenter.onToggleChanged(true, Filter.INTERVENTION_UNIT, InterventionType.PERSON);
        verify(view).onFiltedSelected(2);
        Map<String, Set<String>> filters = Whitebox.getInternalState(filterTasksPresenter, "checkedFilters");
        assertEquals(2, filters.size());
        assertEquals(2, filters.get(Filter.STATUS).size());
        assertEquals(new HashSet<>(Arrays.asList(BusinessStatus.BLOOD_SCREENING_COMPLETE, BusinessStatus.COMPLETE)), filters.get(Filter.STATUS));


        assertEquals(1, filters.get(Filter.INTERVENTION_UNIT).size());
        assertEquals(InterventionType.PERSON, filters.get(Filter.INTERVENTION_UNIT).iterator().next());

        filterTasksPresenter.onToggleChanged(false, Filter.STATUS, BusinessStatus.BLOOD_SCREENING_COMPLETE);
        filters = Whitebox.getInternalState(filterTasksPresenter, "checkedFilters");
        assertEquals(2, filters.size());
        assertEquals(1, filters.get(Filter.STATUS).size());
        assertEquals(BusinessStatus.COMPLETE, filters.get(Filter.STATUS).iterator().next());

    }

    @Test
    public void testGetIntentionTypesMDA() {
        PreferencesUtil.getInstance().setInterventionTypeForPlan(planId, Intervention.MDA);
        assertEquals(Intervention.MDA_INTERVENTIONS, filterTasksPresenter.getIntentionTypes());
    }


    @Test
    public void testGetIntentionTypesFI() {
        PreferencesUtil.getInstance().setInterventionTypeForPlan(planId, Intervention.FI);
        assertEquals(Intervention.FI_INTERVENTIONS, filterTasksPresenter.getIntentionTypes());
    }

    @Test
    public void testGetIntentionTypesIRS() {
        PreferencesUtil.getInstance().setInterventionTypeForPlan(planId, Intervention.IRS);
        assertEquals(Intervention.IRS_INTERVENTIONS, filterTasksPresenter.getIntentionTypes());
    }


    @Test
    public void testGetBusinessStatusOptionsMDA() {
        PreferencesUtil.getInstance().setInterventionTypeForPlan(planId, Intervention.MDA);
        assertEquals(BusinessStatus.MDA_BUSINESS_STATUS, filterTasksPresenter.getBusinessStatusOptions());
    }


    @Test
    public void testGetBusinessStatusOptionsFI() {
        PreferencesUtil.getInstance().setInterventionTypeForPlan(planId, Intervention.FI);
        assertEquals(BusinessStatus.FI_BUSINESS_STATUS, filterTasksPresenter.getBusinessStatusOptions());
    }

    @Test
    public void testGetBusinessStatusOptionsIRS() {
        PreferencesUtil.getInstance().setInterventionTypeForPlan(planId, Intervention.IRS);
        assertEquals(BusinessStatus.IRS_BUSINESS_STATUS, filterTasksPresenter.getBusinessStatusOptions());
    }


    @Test
    public void testOnApplyFilters() {
        filterTasksPresenter.onApplyFilters("Status");
        verify(view).applyFilters(intentArgumentCaptor.capture());
        TaskFilterParams taskFilter = (TaskFilterParams) intentArgumentCaptor.getValue().getSerializableExtra(Filter.FILTER_SORT_PARAMS);
        assertNotNull(taskFilter);
        assertTrue(taskFilter.getCheckedFilters().isEmpty());
        assertEquals("Status", taskFilter.getSortBy());
    }


    @Test
    public void testRestoreCheckedFilters() {
        TaskFilterParams filterParams = TestingUtils.getFilterParams();
        when(view.getBusinessStatusLayout()).thenReturn(flexboxLayout);
        when(view.getTaskCodeLayout()).thenReturn(flexboxLayout);
        when(view.getInterventionTypeLayout()).thenReturn(flexboxLayout);
        when(flexboxLayout.findViewWithTag(any())).thenReturn(toggleButton);
        when(flexboxLayout.getResources()).thenReturn(RuntimeEnvironment.application.getResources());
        filterTasksPresenter.restoreCheckedFilters(filterParams);
        verify(toggleButton, times(3)).setChecked(true);
        verify(view).setSortBySelection(1);

        filterParams.getCheckedFilters().remove(Filter.INTERVENTION_UNIT);
        filterParams.setSortBy("Type");
        filterTasksPresenter.restoreCheckedFilters(filterParams);
        verify(toggleButton, times(3 + 2)).setChecked(true);
        verify(view).setSortBySelection(2);
    }

    @Test
    public void testOnDateSet() {
        filterTasksPresenter.onDateSet(null, 2024, 10, 1);
        verify(view).setFilterFromDate(dateArgumentCaptor.capture());
        Date date = dateArgumentCaptor.getValue();
        assertEquals(1, date.getDate());
        assertEquals(10, date.getMonth());
        assertEquals(124, date.getYear());
    }
}
