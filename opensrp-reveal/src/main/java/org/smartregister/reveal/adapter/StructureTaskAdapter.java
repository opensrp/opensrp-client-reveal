package org.smartregister.reveal.adapter;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.viewholder.StructureTaskViewHolder;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by samuelgithengi on 4/11/19.
 */
public class StructureTaskAdapter extends RecyclerView.Adapter<StructureTaskViewHolder> {

    private Context context;

    private List<StructureTaskDetails> taskDetailsList = new ArrayList<>();
    private View.OnClickListener onClickListener;

    public StructureTaskAdapter(View.OnClickListener onClickListener) {
        this.onClickListener = onClickListener;
    }

    @NonNull
    @Override
    public StructureTaskViewHolder onCreateViewHolder(@NonNull ViewGroup viewGroup, int viewType) {
        context = viewGroup.getContext();
        View view = LayoutInflater.from(viewGroup.getContext()).inflate(R.layout.structure_task_row, viewGroup, false);
        return new StructureTaskViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull StructureTaskViewHolder viewHolder, int position) {
        StructureTaskDetails taskDetails = taskDetailsList.get(position);
        if (Intervention.BEDNET_DISTRIBUTION.equals(taskDetails.getTaskCode())) {
            viewHolder.setTaskName(context.getString(R.string.distribute_llin));
            viewHolder.setTaskAction(context.getString(R.string.record_llin), taskDetails.getBusinessStatus(), onClickListener);
        } else if (Intervention.IRS.equals(taskDetails.getTaskCode())) {
            viewHolder.setTaskName(context.getString(R.string.irs));
            viewHolder.setTaskAction(context.getString(R.string.record_status), taskDetails.getBusinessStatus(), onClickListener);
        } else {

            String action = taskDetails.getTaskAction();
            String name = taskDetails.getTaskName();
            if (Intervention.BLOOD_SCREENING.equals(taskDetails.getTaskCode())) {
                action = context.getString(R.string.record_test);
            } else if (Intervention.CASE_CONFIRMATION.equals(taskDetails.getTaskCode())) {
                action = context.getString(R.string.detect_case);
            } else if (Intervention.REGISTER_FAMILY.equals(taskDetails.getTaskCode())) {
                action = context.getString(R.string.register_family);
                name = context.getString(R.string.family_registration);
            }
            viewHolder.setTaskName(name);
            viewHolder.setTaskAction(action, taskDetails.getBusinessStatus(), onClickListener);
        }

    }

    @Override
    public int getItemCount() {
        return taskDetailsList.size();
    }

    public void setTaskDetailsList(List<StructureTaskDetails> taskDetailsList) {
        this.taskDetailsList = taskDetailsList;
        notifyDataSetChanged();
    }
}
