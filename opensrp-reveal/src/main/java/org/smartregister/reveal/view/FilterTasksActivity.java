package org.smartregister.reveal.view;

import android.os.Bundle;
import android.support.v7.widget.Toolbar;
import android.view.MenuItem;

import org.smartregister.reveal.R;
import org.smartregister.view.activity.MultiLanguageActivity;

public class FilterTasksActivity extends MultiLanguageActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_filter_tasks);

        Toolbar toolbar = this.findViewById(R.id.filter_tasks_toolbar);
        toolbar.setTitle(R.string.filters);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setHomeAsUpIndicator(R.drawable.ic_action_close);

    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }
}
