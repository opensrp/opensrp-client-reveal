package org.smartregister.reveal.fragment;

import android.app.ProgressDialog;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.annotation.StringRes;
import android.support.v4.app.Fragment;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.Toast;

import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.StructureTaskAdapter;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.presenter.StructureTasksPresenter;
import org.smartregister.reveal.util.Utils;

/**
 * Created by samuelgithengi on 4/8/19.
 */
public class StructureTasksFragment extends Fragment implements StructureTasksContract.View {

    private RecyclerView taskRecyclerView;
    private StructureTaskAdapter adapter;

    private StructureTasksContract.Presenter presenter;
    private ProgressDialog progressDialog;

    public static StructureTasksFragment newInstance(Bundle bundle) {
        StructureTasksFragment fragment = new StructureTasksFragment();
        fragment.setPresenter(new StructureTasksPresenter(fragment));
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
        TextView interventionType = view.findViewById(R.id.intervention_type);
        interventionType.setText(getString(Utils.getInterventionLabel()));
        taskRecyclerView = view.findViewById(R.id.task_recyclerView);

        progressDialog = new ProgressDialog(getContext());
        progressDialog.setCancelable(false);
    }

    private View.OnClickListener onClickListener = new View.OnClickListener() {
        @Override
        public void onClick(View view) {
            StructureTaskDetails details = (StructureTaskDetails) view.getTag(R.id.task_details);
            presenter.onTaskSelected(details);
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

    @Override
    public void displayToast(String message) {
        Toast.makeText(getContext(), message, Toast.LENGTH_LONG).show();
    }

    @Override
    public void showProgressDialog(@StringRes int title, @StringRes int message) {
        if (progressDialog != null) {
            progressDialog.setTitle(title);
            progressDialog.setMessage(getString(message));
            progressDialog.show();
        }
    }

    @Override
    public void hideProgressDialog() {
        if (progressDialog != null) {
            progressDialog.dismiss();
        }
    }


    public void setPresenter(StructureTasksContract.Presenter presenter) {
        this.presenter = presenter;
    }
}
