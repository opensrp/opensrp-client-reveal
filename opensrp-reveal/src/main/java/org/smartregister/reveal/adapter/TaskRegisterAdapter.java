package org.smartregister.reveal.adapter;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import org.smartregister.domain.Task;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.BaseCardDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.CardDetailsUtil;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.viewholder.TaskRegisterViewHolder;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Created by samuelgithengi on 3/20/19.
 */
public class TaskRegisterAdapter extends RecyclerView.Adapter<TaskRegisterViewHolder> {

    private List<TaskDetails> taskDetails = new ArrayList<>();

    private Context context;

    private View.OnClickListener registerActionHandler;

    public TaskRegisterAdapter(Context context, View.OnClickListener registerActionHandler) {
        this.context = context;
        this.registerActionHandler = registerActionHandler;
    }

    @NonNull
    @Override
    public TaskRegisterViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.task_register_row, parent, false);
        return new TaskRegisterViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull TaskRegisterViewHolder viewHolder, int position) {
        Random random = new Random();
        TaskDetails task = taskDetails.get(position);
        Float distance = task.getDistanceFromUser();
        String name = task.getStructureName();
        String action = null;
        if (Constants.Intervention.IRS.equals(task.getTaskCode())) {
            if (name == null) {//TODO remove setting structure name with serial numbers once its moved to server
                name = task.getFamilyName() != null ? task.getFamilyName() : "Structure " + random.nextInt(100);
            }
            action = context.getString(R.string.record_status);
        } else if (Constants.Intervention.MOSQUITO_COLLECTION.equals(task.getTaskCode())) {
            name = context.getString(R.string.mosquito_collection_point);
            action = context.getString(R.string.record_mosquito_collection);
        } else if (Constants.Intervention.LARVAL_DIPPING.equals(task.getTaskCode())) {
            name = context.getString(R.string.larval_breeding_site);
            action = context.getString(R.string.record_larvacide);
        } else if (Constants.Intervention.BCC.equals(task.getTaskCode())) {
            viewHolder.setIcon(R.drawable.ic_bcc);
            viewHolder.hideDistanceFromStructure();
            name = context.getString(R.string.bcc);
            action = context.getString(R.string.record_bcc);
        }
        viewHolder.setTaskName(name);
        BaseCardDetails cardDetails = new BaseCardDetails(task.getBusinessStatus());
        if (Task.TaskStatus.COMPLETED.name().equals(task.getTaskStatus())) {
            action = task.getBusinessStatus().replaceAll(" ", "\n");
            CardDetailsUtil.formatCardDetails(cardDetails);
        }
        viewHolder.setTaskAction(action, task, cardDetails, registerActionHandler);
        viewHolder.setDistanceFromStructure(distance);
        viewHolder.setTaskDetails(task.getBusinessStatus(),task.getTaskDetails());
    }

    @Override
    public int getItemCount() {
        return taskDetails.size();
    }

    public void setTaskDetails(List<TaskDetails> taskDetails) {
        this.taskDetails = taskDetails;
        notifyDataSetChanged();
    }
}
