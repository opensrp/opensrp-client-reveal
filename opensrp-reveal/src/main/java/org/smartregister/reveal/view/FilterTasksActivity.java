package org.smartregister.reveal.view;

import android.content.Intent;
import android.os.Bundle;
import androidx.appcompat.view.ContextThemeWrapper;
import androidx.appcompat.widget.AppCompatSpinner;
import androidx.appcompat.widget.Toolbar;
import android.view.MenuItem;
import android.widget.CompoundButton;
import android.widget.TextView;
import android.widget.ToggleButton;

import com.google.android.flexbox.FlexboxLayout;

import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FilterTasksContract;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.presenter.FilterTasksPresenter;
import org.smartregister.reveal.util.Constants.Filter;
import org.smartregister.reveal.util.Constants.InterventionType;
import org.smartregister.view.activity.MultiLanguageActivity;

import java.util.List;

import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;

public class FilterTasksActivity extends MultiLanguageActivity implements FilterTasksContract.View, CompoundButton.OnCheckedChangeListener {

    private FilterTasksContract.Presenter presenter;

    private AppCompatSpinner sortBySpinner;

    private FlexboxLayout businessStatusLayout;

    private FlexboxLayout taskCodeLayout;

    private FlexboxLayout interventionTypeLayout;

    private TextView applyFiltersTextView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        presenter = new FilterTasksPresenter(this);
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

        setUpToggleButtonGroups();

        findViewById(R.id.clear_filters).setOnClickListener(view -> {
            clearSelections();
        });

        applyFiltersTextView.setOnClickListener(view -> {
            presenter.onApplyFilters(sortBySpinner.getSelectedItem().toString());
        });


        TaskFilterParams filterParams = (TaskFilterParams) getIntent().getSerializableExtra(Filter.FILTER_SORT_PARAMS);
        if (filterParams != null) {
            presenter.restoreCheckedFilters(filterParams);
        }
    }


    private void clearSelections() {
        sortBySpinner.setSelection(0);
        clearSelectedButtons(businessStatusLayout);
        clearSelectedButtons(taskCodeLayout);
        clearSelectedButtons(interventionTypeLayout);
    }


    private void registerCheckedChangeListener() {
        registerCheckedChangeListener(businessStatusLayout, Filter.STATUS);
        registerCheckedChangeListener(taskCodeLayout, Filter.CODE);
        registerCheckedChangeListener(interventionTypeLayout, Filter.INTERVENTION_UNIT);
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

    private void setUpToggleButtonGroups() {
        populateToggleButtons(businessStatusLayout, presenter.getBusinessStatusOptions());
        populateToggleButtons(taskCodeLayout, presenter.getIntentionTypes());
        populateToggleButtons(interventionTypeLayout, InterventionType.FILTERABLE_INTERVENTION_TYPES);
        registerCheckedChangeListener();

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
    public void setSortBySelection(int sortBySpinner) {
        this.sortBySpinner.setSelection(sortBySpinner);
    }
}
