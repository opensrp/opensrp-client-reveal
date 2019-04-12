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
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.presenter.StructureTasksPresenter;
import org.smartregister.reveal.util.Utils;

/**
 * Created by samuelgithengi on 4/8/19.
 */
public class StructureTasksFragment extends Fragment implements StructureTasksContract.View {

    private RecyclerView taskRecyclerView;
    private StructureTaskAdapter adapter;

    private StructureTasksContract.Presenter presenter;

    public static StructureTasksFragment newInstance(Bundle bundle) {
        StructureTasksFragment fragment = new StructureTasksFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        presenter = new StructureTasksPresenter(this);
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
        TextView interventionType = view.findViewById(R.id.intervention_type);
        interventionType.setText(getString(Utils.getInterventionLabel()));
        taskRecyclerView = view.findViewById(R.id.task_recyclerView);
    }

    private View.OnClickListener onClickListener = new View.OnClickListener() {
        @Override
        public void onClick(View v) {

        }
    };

    @Override
    public StructureTaskAdapter getAdapter() {
        return adapter;
    }

    @Override
    public void setStructure(String structureId) {
        presenter.findTasks(structureId);
    }
}
