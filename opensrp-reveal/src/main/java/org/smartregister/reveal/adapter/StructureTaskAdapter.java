package org.smartregister.reveal.adapter;

import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.viewholder.StructureTaskViewHolder;

import java.util.List;

/**
 * Created by samuelgithengi on 4/11/19.
 */
public class StructureTaskAdapter extends RecyclerView.Adapter<StructureTaskViewHolder> {


    private List<StructureTaskDetails> taskDetailsList;
    private View.OnClickListener onClickListener;

    public StructureTaskAdapter(View.OnClickListener onClickListener) {
        this.onClickListener = onClickListener;
    }

    @NonNull
    @Override
    public StructureTaskViewHolder onCreateViewHolder(@NonNull ViewGroup viewGroup, int viewType) {
        View view = LayoutInflater.from(viewGroup.getContext()).inflate(R.layout.structure_task_row, viewGroup, false);
        return new StructureTaskViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull StructureTaskViewHolder viewHolder, int position) {
        StructureTaskDetails taskDetails = taskDetailsList.get(position);
        viewHolder.setTaskName(taskDetails.getTaskName());
        viewHolder.setTaskAction(taskDetails.getTaskAction(), taskDetails.getBusinessStatus(),onClickListener);

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
