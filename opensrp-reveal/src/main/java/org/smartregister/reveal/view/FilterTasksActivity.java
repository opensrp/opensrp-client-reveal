package org.smartregister.reveal.view;

import android.app.DatePickerDialog;
import android.content.Intent;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.TextView;
import android.widget.ToggleButton;

import androidx.appcompat.view.ContextThemeWrapper;
import androidx.appcompat.widget.AppCompatSpinner;
import androidx.appcompat.widget.Toolbar;

import com.google.android.flexbox.FlexboxLayout;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FilterTasksContract;
import org.smartregister.reveal.model.FilterConfiguration;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.presenter.FilterTasksPresenter;
import org.smartregister.reveal.util.Constants.Filter;
import org.smartregister.reveal.util.Constants.InterventionType;
import org.smartregister.view.activity.MultiLanguageActivity;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import timber.log.Timber;

import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;
import static org.smartregister.reveal.util.Constants.Filter.FILTER_CONFIGURATION;

public class FilterTasksActivity extends MultiLanguageActivity implements FilterTasksContract.View, CompoundButton.OnCheckedChangeListener {

    public static final SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy", Locale.ENGLISH);

    private FilterTasksContract.Presenter presenter;

    private AppCompatSpinner sortBySpinner;

    private FlexboxLayout businessStatusLayout;

    private FlexboxLayout taskCodeLayout;

    private FlexboxLayout interventionTypeLayout;

    private FlexboxLayout formNameLayout;

    private TextView applyFiltersTextView;

    private TextView fromDateFilterTextView;

    private CheckBox viewAllEventsCheckBox;

    private FilterConfiguration filterConfiguration;

    private DatePickerDialog datePickerDialog;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        filterConfiguration = (FilterConfiguration) getIntent().getSerializableExtra(FILTER_CONFIGURATION);
        presenter = new FilterTasksPresenter(this, filterConfiguration);
        setContentView(R.layout.activity_filter_tasks);
        Toolbar toolbar = this.findViewById(R.id.filter_tasks_toolbar);
        toolbar.setTitle(R.string.filter);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setHomeAsUpIndicator(R.drawable.ic_action_close);


        sortBySpinner = findViewById(R.id.sort_by);
        businessStatusLayout = findViewById(R.id.business_status_layout);
        taskCodeLayout = findViewById(R.id.task_code_layout);
        interventionTypeLayout = findViewById(R.id.intervention_type_layout);
        applyFiltersTextView = findViewById(R.id.apply_filters);
        formNameLayout = findViewById(R.id.form_name_layout);
        fromDateFilterTextView = findViewById(R.id.filter_from_date);
        viewAllEventsCheckBox = findViewById(R.id.view_all_events);

        setSortOptions();
        setFilterVisibility();
        setUpToggleButtonGroups();
        registerCheckedChangeListener();

        findViewById(R.id.clear_filters).setOnClickListener(view -> {
            clearSelections();
        });

        applyFiltersTextView.setOnClickListener(view -> {
            presenter.onApplyFilters(sortBySpinner.getSelectedItem().toString());
        });

        Calendar cal = Calendar.getInstance();
        datePickerDialog = new DatePickerDialog(
                this, presenter, cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH));
        fromDateFilterTextView.setOnClickListener(view -> {
            datePickerDialog.show();
        });


        TaskFilterParams filterParams = (TaskFilterParams) getIntent().getSerializableExtra(Filter.FILTER_SORT_PARAMS);
        if (filterParams != null) {
            presenter.restoreCheckedFilters(filterParams);
        }

    }

    private void setSortOptions() {
        if (filterConfiguration != null && filterConfiguration.getSortOptions() != null) {
            String[] sortOptions = getResources().getStringArray(filterConfiguration.getSortOptions());
            ArrayAdapter<String> adapter = new ArrayAdapter<>(
                    this, android.R.layout.simple_spinner_item, sortOptions);
            adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
            sortBySpinner.setAdapter(adapter);
        }
    }

    private void clearSelections() {
        sortBySpinner.setSelection(0);
        clearSelectedButtons(businessStatusLayout);
        clearSelectedButtons(taskCodeLayout);
        clearSelectedButtons(interventionTypeLayout);
        clearSelectedButtons(formNameLayout);
        clearDatePicker();
        presenter.onClearSelections();
    }


    private void registerCheckedChangeListener() {
        registerCheckedChangeListener(businessStatusLayout, Filter.STATUS);
        registerCheckedChangeListener(taskCodeLayout, Filter.CODE);
        registerCheckedChangeListener(interventionTypeLayout, Filter.INTERVENTION_UNIT);
        registerCheckedChangeListener(formNameLayout, Filter.FORM_NAME);
    }

    private void registerCheckedChangeListener(FlexboxLayout layout, String category) {
        for (int i = 0; i < layout.getFlexItemCount(); i++) {
            ToggleButton toggleButton = ((ToggleButton) layout.getFlexItemAt(i));
            toggleButton.setOnCheckedChangeListener(this);
            toggleButton.setTag(R.id.filter_category, category);
        }
    }

    private void clearSelectedButtons(FlexboxLayout layout) {
        for (int i = 0; i < layout.getFlexItemCount(); i++) {
            ((ToggleButton) layout.getFlexItemAt(i)).setChecked(false);
        }
    }

    private void clearDatePicker() {
        if (datePickerDialog != null) {
            Calendar cal = Calendar.getInstance();
            datePickerDialog.updateDate(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH));
            fromDateFilterTextView.setText(getString(R.string.date_format));
        }
    }

    private void setUpToggleButtonGroups() {
        populateToggleButtons(businessStatusLayout, presenter.getBusinessStatusOptions());
        populateToggleButtons(taskCodeLayout, presenter.getIntentionTypes());
        populateToggleButtons(interventionTypeLayout, InterventionType.FILTERABLE_INTERVENTION_TYPES);
        if (filterConfiguration.isFormsLayoutEnabled() && filterConfiguration.getEventTypeList() != null) {
            populateToggleButtons(formNameLayout, filterConfiguration.getEventTypeList());
        }
    }


    private void setFilterVisibility() {
        setViewVisibility(findViewById(R.id.status_group), filterConfiguration.isBusinessStatusLayoutEnabled());
        setViewVisibility(findViewById(R.id.task_type_group), filterConfiguration.isTaskCodeLayoutEnabled());
        setViewVisibility(findViewById(R.id.intervention_group), filterConfiguration.isInterventionTypeLayoutEnabled());
        setViewVisibility(findViewById(R.id.form_name_group), filterConfiguration.isFormsLayoutEnabled());
        setViewVisibility(findViewById(R.id.filter_date_group), filterConfiguration.isFilterFromDateAndViewAllEventsEnabled());
    }

    private void setViewVisibility(View view, boolean visible) {
        if (view != null)
            view.setVisibility(visible ? View.VISIBLE : View.GONE);
    }


    private void populateToggleButtons(FlexboxLayout layout, List<String> options) {
        FlexboxLayout.LayoutParams params = new FlexboxLayout.LayoutParams(WRAP_CONTENT, WRAP_CONTENT);
        params.setMargins(0, 0, getResources().getDimensionPixelSize(R.dimen.filter_toggle_end_margin),
                getResources().getDimensionPixelSize(R.dimen.filter_toggle_bottom_margin));
        for (String intervention : options) {

            ToggleButton toggleButton = new ToggleButton(new ContextThemeWrapper(this, R.style.TaskFilterToggle), null, 0);
            Integer label = presenter.getStringResource(intervention);
            if (label == null) {
                toggleButton.setText(intervention);
            } else {
                toggleButton.setText(label);
            }
            toggleButton.setBackgroundDrawable(getResources().getDrawable(R.drawable.toggle_bg));
            toggleButton.setTag(intervention);
            layout.addView(toggleButton, params);
        }
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {

        presenter.onToggleChanged(buttonView.isChecked(), buttonView.getTag(R.id.filter_category), buttonView.getTag());
    }

    @Override
    public void onFiltedSelected(int size) {
        applyFiltersTextView.setText(getString(R.string.apply_filters_formatter, size));
    }

    @Override
    public void applyFilters(Intent intent) {
        setResult(RESULT_OK, intent);
        finish();
    }

    @Override
    public FlexboxLayout getBusinessStatusLayout() {
        return businessStatusLayout;
    }

    @Override
    public FlexboxLayout getTaskCodeLayout() {
        return taskCodeLayout;
    }

    @Override
    public FlexboxLayout getInterventionTypeLayout() {
        return interventionTypeLayout;
    }

    @Override
    public FlexboxLayout getFormNameLayout() {
        return formNameLayout;
    }

    @Override
    public Date getFromDateFilter() {
        CharSequence date = fromDateFilterTextView.getText();
        if (StringUtils.isNotBlank(date)) {
            try {
                return dateFormat.parse(date.toString());
            } catch (ParseException e) {
                Timber.e(e);
            }
        }
        return null;
    }

    @Override
    public boolean viewAllEvents() {
        return viewAllEventsCheckBox.isChecked();
    }

    @Override
    public void setViewAllEvents(boolean viewAllEvents) {
        viewAllEventsCheckBox.setChecked(viewAllEvents);
    }

    @Override
    public void setFilterFromDate(Date dateFrom) {
        fromDateFilterTextView.setText(dateFormat.format(dateFrom));
    }

    @Override
    public void setSortBySelection(int sortBySpinner) {
        this.sortBySpinner.setSelection(sortBySpinner);
    }
}
