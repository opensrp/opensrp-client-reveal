package org.smartregister.reveal.view;

import android.os.Bundle;
import android.support.v7.view.ContextThemeWrapper;
import android.support.v7.widget.AppCompatSpinner;
import android.support.v7.widget.Toolbar;
import android.view.MenuItem;
import android.widget.CompoundButton;
import android.widget.TextView;
import android.widget.ToggleButton;

import com.google.android.flexbox.FlexboxLayout;

import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FilterTasksContract;
import org.smartregister.reveal.presenter.FilterTasksPresenter;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.view.activity.MultiLanguageActivity;

public class FilterTasksActivity extends MultiLanguageActivity implements FilterTasksContract.View, CompoundButton.OnCheckedChangeListener {

    private FilterTasksContract.Presenter presenter;

    private AppCompatSpinner sortBy;

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
        toolbar.setTitle(R.string.filters);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setHomeAsUpIndicator(R.drawable.ic_action_close);

        sortBy = findViewById(R.id.sort_by);
        businessStatusLayout = findViewById(R.id.business_status_layout);
        taskCodeLayout = findViewById(R.id.task_code_layout);
        interventionTypeLayout = findViewById(R.id.intervention_type_layout);
        applyFiltersTextView = findViewById(R.id.apply_filters);

        setUpToggleButtonGroups();

        findViewById(R.id.clear_filters).setOnClickListener((view) -> {
            clearSelections();
        });
    }


    private void clearSelections() {
        sortBy.setSelection(0);
        clearSelectedButtons(businessStatusLayout);
        clearSelectedButtons(taskCodeLayout);
        clearSelectedButtons(interventionTypeLayout);
    }


    private void registerCheckedChangeListener() {
        registerCheckedChangeListener(businessStatusLayout, Constants.Filter.STATUS);
        registerCheckedChangeListener(taskCodeLayout, Constants.Filter.CODE);
        registerCheckedChangeListener(interventionTypeLayout, Constants.Filter.INTERVENTION_UNIT);
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
        FlexboxLayout taskCodeLayout = findViewById(R.id.task_code_layout);
        populateToggleButtons(taskCodeLayout);
        registerCheckedChangeListener();

    }

    private void populateToggleButtons(FlexboxLayout layout) {
        FlexboxLayout.LayoutParams params = (FlexboxLayout.LayoutParams) findViewById(R.id.toggle_sprayed).getLayoutParams();
        for (String intervention : Intervention.NON_AGGREGATE_INTERVENTIONS) {

            ToggleButton toggleButton = new ToggleButton(new ContextThemeWrapper(this, R.style.TaskFilterToggle), null, 0);
            Integer label = presenter.getStringResource(intervention);
            if (label == null) {
                toggleButton.setText(intervention);
            } else {
                toggleButton.setText(label);
            }
            toggleButton.setBackgroundDrawable(getResources().getDrawable(R.drawable.toggle_bg));
            toggleButton.setTag(R.id.filter_key, intervention);
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

        presenter.onToggleChanged(buttonView.getId(), buttonView.getTag(R.id.filter_category), buttonView.getTag(R.id.filter_key));
    }

    @Override
    public void onFiltedSelected(int size) {
        applyFiltersTextView.setText(getString(R.string.apply_filters_formatter, size));
    }
}
