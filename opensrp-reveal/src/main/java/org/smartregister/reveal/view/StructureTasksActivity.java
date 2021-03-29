package org.smartregister.reveal.view;

import android.os.Bundle;

import androidx.fragment.app.FragmentTransaction;

import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.StructureTasksFragment;
import org.smartregister.reveal.util.Constants;
import org.smartregister.view.activity.MultiLanguageActivity;

public class StructureTasksActivity extends MultiLanguageActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_structure_tasks);
        StructureTasksFragment structureTasksFragment = StructureTasksFragment.newInstance(this.getIntent().getExtras(), this);
        FragmentTransaction ft = getSupportFragmentManager().beginTransaction();
        ft.add(R.id.fragment_container, structureTasksFragment).commit();
        getSupportFragmentManager().executePendingTransactions();
        structureTasksFragment.setStructure(getIntent().getStringExtra(Constants.Properties.LOCATION_UUID));
    }
}