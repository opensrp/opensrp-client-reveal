package org.smartregister.reveal.presenter;

import android.content.Intent;
import android.view.ViewGroup;
import android.widget.DatePicker;
import android.widget.ToggleButton;

import androidx.annotation.StringRes;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FilterTasksContract;
import org.smartregister.reveal.model.FilterConfiguration;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.EventType;
import org.smartregister.reveal.util.Constants.Filter;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Constants.InterventionType;
import org.smartregister.reveal.util.Utils;

import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by samuelgithengi on 12/18/19.
 */
public class FilterTasksPresenter implements FilterTasksContract.Presenter {

    private FilterTasksContract.View view;

    private Map<String, Integer> labelsMap;

    private Map<String, Set<String>> checkedFilters = new HashMap<>();

    private FilterConfiguration filterConfiguration;

    private Calendar fromDateFilter;

    public FilterTasksPresenter(FilterTasksContract.View view, FilterConfiguration filterConfiguration) {
        this.view = view;
        this.filterConfiguration = filterConfiguration;
        populateLabels();
    }


    private void populateLabels() {
        labelsMap = new HashMap<>();
        //Interventions
        labelsMap.put(Intervention.IRS, R.string.irs);
        labelsMap.put(Intervention.MOSQUITO_COLLECTION, R.string.mosquito_collection);
        labelsMap.put(Intervention.LARVAL_DIPPING, R.string.larval_dipping);
        labelsMap.put(Intervention.BCC, R.string.bcc_code);
        labelsMap.put(Intervention.BEDNET_DISTRIBUTION, R.string.bednet_distribution);
        labelsMap.put(Intervention.BLOOD_SCREENING, R.string.blood_screening);
        labelsMap.put(Intervention.CASE_CONFIRMATION, R.string.case_confirmation);
        labelsMap.put(Intervention.REGISTER_FAMILY, R.string.register_family);
        labelsMap.put(Intervention.PAOT, R.string.paot);
        labelsMap.put(Intervention.MDA_DISPENSE, R.string.mda_dispense);
        labelsMap.put(Intervention.MDA_ADHERENCE, R.string.mda_adherence);
        labelsMap.put(Intervention.IRS_VERIFICATION, R.string.irs_verification);

        //Intervention Types
        labelsMap.put(InterventionType.OPERATIONAL_AREA, R.string.operational_area);
        labelsMap.put(InterventionType.STRUCTURE, R.string.structure);
        labelsMap.put(InterventionType.FAMILY, R.string.family);
        labelsMap.put(InterventionType.PERSON, R.string.person);

        //Business status
        labelsMap.put(BusinessStatus.NOT_VISITED, R.string.not_visited);
        labelsMap.put(BusinessStatus.NOT_SPRAYED, R.string.not_sprayed);
        labelsMap.put(BusinessStatus.NOT_SPRAYABLE, R.string.not_sprayable);
        labelsMap.put(BusinessStatus.SPRAYED, R.string.sprayed);

        labelsMap.put(BusinessStatus.COMPLETE, R.string.complete);
        labelsMap.put(BusinessStatus.INCOMPLETE, R.string.incomplete);
        labelsMap.put(BusinessStatus.NOT_ELIGIBLE, R.string.not_eligible);
        labelsMap.put(BusinessStatus.IN_PROGRESS, R.string.in_progress);

        //Forms
        labelsMap.put(Constants.SPRAY_EVENT, R.string.hh_form);
        labelsMap.put(EventType.VERIFICATION_EVENT, R.string.verification_form_name);
        labelsMap.put(EventType.DAILY_SUMMARY_EVENT, R.string.daily_summary_name);
        labelsMap.put(EventType.TEAM_LEADER_DOS_EVENT, R.string.team_leader_dos_form);
        labelsMap.put(EventType.MOBILIZATION_EVENT, R.string.mobilization_form_name);
        labelsMap.put(EventType.IRS_SA_DECISION_EVENT, R.string.irs_field_officer_form);
        labelsMap.put(EventType.IRS_FIELD_OFFICER_EVENT, R.string.irs_field_officer_form);
        labelsMap.put(EventType.CB_SPRAY_AREA_EVENT, R.string.cb_spray_area_form);
        labelsMap.put(EventType.IRS_LITE_VERIFICATION, R.string.irs_lite_verification_form_name);

        // Following are for grouped structure tasks.
        labelsMap.put(BusinessStatus.FAMILY_REGISTERED, R.string.family_registered);
        labelsMap.put(BusinessStatus.BEDNET_DISTRIBUTED, R.string.bednet_distributed);
        labelsMap.put(BusinessStatus.BLOOD_SCREENING_COMPLETE, R.string.blood_screening_complete);
    }


    @Override
    public @StringRes
    Integer getStringResource(String intervention) {
        return labelsMap.get(intervention);
    }

    @Override
    public void onToggleChanged(boolean isChecked, Object filterCategory, Object filterKey) {
        if (filterCategory != null) {
            Set<String> selected = checkedFilters.get(filterCategory.toString());
            if (selected == null) {
                selected = new HashSet<>();
                checkedFilters.put(filterCategory.toString(), selected);
            }
            if (isChecked) {
                selected.add(filterKey.toString());
            } else {
                selected.remove(filterKey.toString());
                if (selected.isEmpty()) {
                    checkedFilters.remove(filterCategory.toString());
                }
            }
        }
        view.onFiltedSelected(checkedFilters.size());
    }

    @Override
    public List<String> getIntentionTypes() {
        if (Utils.isMDA()) {
            return Intervention.MDA_INTERVENTIONS;
        } else if (Utils.isFocusInvestigation()) {
            return Intervention.FI_INTERVENTIONS;
        } else
            return Intervention.IRS_INTERVENTIONS;
    }

    @Override
    public List<String> getBusinessStatusOptions() {
        return filterConfiguration.getBusinessStatusList() != null ? filterConfiguration.getBusinessStatusList() :
                Utils.isFocusInvestigation() ? BusinessStatus.FI_BUSINESS_STATUS : Utils.isMDA() ? BusinessStatus.MDA_BUSINESS_STATUS : BusinessStatus.IRS_BUSINESS_STATUS;
    }

    @Override
    public void onApplyFilters(String selectedItem) {
        Intent intent = new Intent();
        intent.putExtra(Filter.FILTER_SORT_PARAMS, TaskFilterParams.builder().sortBy(selectedItem)
                .checkedFilters(checkedFilters).fromDate(view.getFromDateFilter())
                .viewAllEvents(view.viewAllEvents()).fromDate(fromDateFilter == null ? null : fromDateFilter.getTime()).build());

        view.applyFilters(intent);
    }

    @Override
    public void restoreCheckedFilters(TaskFilterParams taskFilterParams) {
        if (taskFilterParams != null) {
            checkedFilters = taskFilterParams.getCheckedFilters();
            restoreSelections(checkedFilters.get(Constants.Filter.STATUS), view.getBusinessStatusLayout());
            restoreSelections(checkedFilters.get(Constants.Filter.CODE), view.getTaskCodeLayout());
            restoreSelections(checkedFilters.get(Filter.INTERVENTION_UNIT), view.getInterventionTypeLayout());
            restoreSelections(checkedFilters.get(Filter.FORM_NAME), view.getFormNameLayout());
            if (StringUtils.isNotBlank(taskFilterParams.getSortBy())) {
                int index = Arrays.asList(view.getBusinessStatusLayout().getResources().getStringArray(
                        filterConfiguration.getSortOptions() == null ? R.array.task_sort_options : filterConfiguration.getSortOptions()))
                        .indexOf(taskFilterParams.getSortBy());
                view.setSortBySelection(index == -1 ? 0 : index);
            }
            if (taskFilterParams.getFromDate() != null) {
                view.setFilterFromDate(taskFilterParams.getFromDate());
                fromDateFilter = Calendar.getInstance();
                fromDateFilter.setTime(taskFilterParams.getFromDate());
            }
            view.setViewAllEvents(taskFilterParams.isViewAllEvents());

        }
    }

    @Override
    public void onClearSelections() {
        fromDateFilter = null;
    }

    private void restoreSelections(Set<String> filters, ViewGroup viewGroup) {
        if (filters == null)
            return;
        for (String filter : filters) {
            ToggleButton toggleButton = viewGroup.findViewWithTag(filter);
            if (toggleButton != null)
                toggleButton.setChecked(true);
        }
    }

    @Override
    public void onDateSet(DatePicker datePicker, int year, int month, int dayOfMonth) {
        fromDateFilter = Calendar.getInstance();
        fromDateFilter.set(year, month, dayOfMonth);
        view.setFilterFromDate(fromDateFilter.getTime());
    }
}
