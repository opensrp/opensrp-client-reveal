package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.StructureTaskAdapter;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * Created by samuelgithengi on 4/8/19.
 */
public class StructureTasksFragment extends Fragment {

    private TextView interventionType;
    private RecyclerView taskRecyclerView;
    private StructureTaskAdapter adapter;

    public static StructureTasksFragment newInstance(Bundle bundle) {
        StructureTasksFragment fragment = new StructureTasksFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        return fragment;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_structure_task, container, false);
        setUpViews(view);
        initializeAdapter();
        return view;
    }

    private void initializeAdapter() {
        adapter = new StructureTaskAdapter(onClickListener);
        taskRecyclerView.setAdapter(adapter);
    }

    private void setUpViews(View view) {
        interventionType = view.findViewById(R.id.intervention_type);
        interventionType.setText(getString(Utils.getInterventionLabel()));
        taskRecyclerView = view.findViewById(R.id.task_recyclerView);
    }


    @Override
    public void onResume() {
        super.onResume();
        List<StructureTaskDetails> taskDetailsList = new ArrayList<>();
        StructureTaskDetails details = new StructureTaskDetails(UUID.randomUUID().toString());
        details.setTaskName("Distibute LLIN");
        details.setTaskAction("Record LLIN");
        taskDetailsList.add(details);

        details = new StructureTaskDetails(UUID.randomUUID().toString());
        details.setTaskName("Bob Smith, 33");
        details.setTaskAction("Detect Case");
        taskDetailsList.add(details);
        adapter.setTaskDetailsList(taskDetailsList);

    }

    private View.OnClickListener onClickListener = new View.OnClickListener() {
        @Override
        public void onClick(View v) {

        }
    };
}
