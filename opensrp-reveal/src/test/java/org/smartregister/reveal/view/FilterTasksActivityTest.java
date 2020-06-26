package org.smartregister.reveal.view;

import android.content.Intent;
import androidx.appcompat.widget.AppCompatSpinner;
import android.widget.TextView;
import android.widget.ToggleButton;

import com.google.android.flexbox.FlexboxLayout;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.Shadows;
import org.robolectric.shadows.ShadowActivity;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FilterTasksContract;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.Filter;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.InterventionType.STRUCTURE;

/**
 * Created by samuelgithengi on 1/28/20.
 */
public class FilterTasksActivityTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private FilterTasksContract.Presenter presenter;

    private FilterTasksActivity filterTasksActivity;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        filterTasksActivity = Robolectric.buildActivity(FilterTasksActivity.class).create().resume().get();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(filterTasksActivity);
    }

    @Test
    public void testClearSelections() {
        FlexboxLayout businessStatusLayout = filterTasksActivity.findViewById(R.id.business_status_layout);
        for (int i = 0; i < businessStatusLayout.getFlexItemCount(); i++) {
            ((ToggleButton) businessStatusLayout.getFlexItemAt(i)).setChecked(true);
        }

        AppCompatSpinner sortBySpinner = filterTasksActivity.findViewById(R.id.sort_by);
        sortBySpinner.setSelection(2);

        filterTasksActivity.findViewById(R.id.clear_filters).performClick();

        assertEquals(0, sortBySpinner.getSelectedItemPosition());
        for (int i = 0; i < businessStatusLayout.getFlexItemCount(); i++) {
            assertFalse(((ToggleButton) businessStatusLayout.getFlexItemAt(i)).isChecked());
        }
    }


    @Test
    public void testApplyFiltersClicked() {

        Whitebox.setInternalState(filterTasksActivity, "presenter", presenter);
        AppCompatSpinner sortBySpinner = filterTasksActivity.findViewById(R.id.sort_by);
        sortBySpinner.setSelection(2);

        filterTasksActivity.findViewById(R.id.apply_filters).performClick();

        verify(presenter).onApplyFilters(filterTasksActivity.getResources().getStringArray(R.array.task_sort_options)[2]);
    }


    @Test
    public void testFiltersStoredOnCreation() {
        Intent intent = new Intent();
        TaskFilterParams filterParams = TestingUtils.getFilterParams();
        intent.putExtra(Filter.FILTER_SORT_PARAMS, filterParams);
        filterTasksActivity = Robolectric.buildActivity(FilterTasksActivity.class, intent).create().resume().get();


        AppCompatSpinner sortBySpinner = filterTasksActivity.findViewById(R.id.sort_by);
        assertEquals(1, sortBySpinner.getSelectedItemPosition());
        assertOnlyFilterISChecked(filterTasksActivity.findViewById(R.id.business_status_layout), NOT_VISITED);
        assertOnlyFilterISChecked(filterTasksActivity.findViewById(R.id.task_code_layout), IRS);
        assertOnlyFilterISChecked(filterTasksActivity.findViewById(R.id.intervention_type_layout), STRUCTURE);

    }

    private void assertOnlyFilterISChecked(FlexboxLayout flexboxLayout, String activeFilter) {
        for (int i = 0; i < flexboxLayout.getFlexItemCount(); i++) {
            ToggleButton toggleButton = (ToggleButton) flexboxLayout.getFlexItemAt(i);
            assertTrue(!toggleButton.isChecked() || activeFilter.equals(toggleButton.getTag()));
        }
    }

    @Test
    public void testOnClickReturnHome() {
        ShadowActivity shadowActivity = Shadows.shadowOf(filterTasksActivity);
        shadowActivity.clickMenuItem(android.R.id.home);
        assertTrue(filterTasksActivity.isFinishing());
    }

    @Test
    public void testOnCheckedChanged() {
        ToggleButton toggleButton = new ToggleButton(RuntimeEnvironment.application);
        toggleButton.setTag(BusinessStatus.BEDNET_DISTRIBUTED);
        toggleButton.setTag(R.id.filter_category, Filter.STATUS);
        toggleButton.setChecked(true);
        Whitebox.setInternalState(filterTasksActivity, "presenter", presenter);
        filterTasksActivity.onCheckedChanged(toggleButton, true);
        verify(presenter).onToggleChanged(true, Filter.STATUS, BusinessStatus.BEDNET_DISTRIBUTED);
    }


    @Test
    public void testOnFiltedSelected() {
        filterTasksActivity.onFiltedSelected(2);
        assertEquals("Apply Filters (2)", ((TextView) filterTasksActivity.findViewById(R.id.apply_filters)).getText().toString());
    }

    @Test
    public void testApplyFilters() {
        filterTasksActivity.applyFilters(null);
        assertTrue(filterTasksActivity.isFinishing());
    }

    @Test
    public void testGetBusinessStatusLayout() {
        assertEquals(filterTasksActivity.findViewById(R.id.business_status_layout), filterTasksActivity.getBusinessStatusLayout());
    }

    @Test
    public void testGetTaskCodeLayout() {
        assertEquals(filterTasksActivity.findViewById(R.id.task_code_layout), filterTasksActivity.getTaskCodeLayout());
    }


    @Test
    public void testGetInterventionTypeLayout() {
        assertEquals(filterTasksActivity.findViewById(R.id.intervention_type_layout), filterTasksActivity.getInterventionTypeLayout());
    }

    @Test
    public void testSetSortBySelection() {
        filterTasksActivity.setSortBySelection(1);
        AppCompatSpinner sortBySpinner = filterTasksActivity.findViewById(R.id.sort_by);
        assertEquals(1, sortBySpinner.getSelectedItemPosition());
    }


}
