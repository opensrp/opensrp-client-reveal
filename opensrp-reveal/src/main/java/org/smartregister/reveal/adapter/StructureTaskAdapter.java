package org.smartregister.reveal.adapter;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import org.smartregister.domain.Task;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.viewholder.StructureTaskViewHolder;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

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
            taskDetails.setTaskName(context.getString(R.string.distribute_llin));
            taskDetails.setTaskAction(context.getString(R.string.record_llin));
        } else if (Intervention.IRS.equals(taskDetails.getTaskCode())) {
            taskDetails.setTaskName(context.getString(R.string.irs));
            taskDetails.setTaskAction(context.getString(R.string.record_status));
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
            taskDetails.setTaskName(name);
            taskDetails.setTaskAction(action);

        }
        viewHolder.setTaskName(taskDetails.getTaskName());
        viewHolder.setTaskAction(taskDetails, onClickListener);

    }

    @Override
    public int getItemCount() {
        return taskDetailsList.size();
    }

    public void setTaskDetailsList(List<StructureTaskDetails> taskDetailsList) {
        this.taskDetailsList = taskDetailsList;
        notifyDataSetChanged();
    }

    private int updateTaskStatus(String taskID, Task.TaskStatus taskStatus, String businessStatus) {
        int position = taskDetailsList.indexOf(new StructureTaskDetails(taskID));
        StructureTaskDetails taskDetails = taskDetailsList.get(position);
        taskDetails.setBusinessStatus(businessStatus);
        taskDetails.setTaskStatus(taskStatus.name());
        return position;
    }

    public void updateTask(String taskID, Task.TaskStatus taskStatus, String businessStatus) {
        notifyItemChanged(updateTaskStatus(taskID, taskStatus, businessStatus));
    }

    public void updateTasks(String taskID, Task.TaskStatus taskStatus, String businessStatus, Set<Task> removedTasks) {
        updateTaskStatus(taskID, taskStatus, businessStatus);
        for (Task task : removedTasks) {
            taskDetailsList.remove(new StructureTaskDetails(task.getIdentifier()));
        }
        notifyDataSetChanged();
    }
}
