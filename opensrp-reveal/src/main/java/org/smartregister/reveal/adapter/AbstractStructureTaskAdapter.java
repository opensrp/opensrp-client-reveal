package org.smartregister.reveal.adapter;

import androidx.recyclerview.widget.RecyclerView;

import org.smartregister.domain.Task;
import org.smartregister.reveal.model.StructureTaskDetails;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public abstract class AbstractStructureTaskAdapter<VH extends RecyclerView.ViewHolder> extends RecyclerView.Adapter<VH> {

    protected List<StructureTaskDetails> taskDetailsList = new ArrayList<>();


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
        if (position != -1) {
            StructureTaskDetails taskDetails = taskDetailsList.get(position);
            taskDetails.setBusinessStatus(businessStatus);
            taskDetails.setTaskStatus(taskStatus.name());
        }
        return position;
    }

    public void updateTask(String taskID, Task.TaskStatus taskStatus, String businessStatus) {
        int position = updateTaskStatus(taskID, taskStatus, businessStatus);
        if (position != -1) {
            notifyItemChanged(position);
        }
    }

    public void updateTasks(String taskID, Task.TaskStatus taskStatus, String businessStatus, Set<Task> removedTasks) {
        updateTaskStatus(taskID, taskStatus, businessStatus);
        for (Task task : removedTasks) {
            taskDetailsList.remove(new StructureTaskDetails(task.getIdentifier()));
        }
        notifyDataSetChanged();
    }
}
