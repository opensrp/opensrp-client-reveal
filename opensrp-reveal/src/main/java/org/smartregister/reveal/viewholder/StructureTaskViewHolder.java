package org.smartregister.reveal.viewholder;

import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.widget.TextView;

import org.smartregister.reveal.R;

/**
 * Created by samuelgithengi on 4/11/19.
 */
public class StructureTaskViewHolder extends RecyclerView.ViewHolder {

    private TextView nameTextView;

    private TextView actionTextView;

    public StructureTaskViewHolder(@NonNull View itemView) {
        super(itemView);
        nameTextView = itemView.findViewById(R.id.task_name);
        actionTextView = itemView.findViewById(R.id.task_action);
    }

    public void setTaskName(String name) {
        nameTextView.setText(name);
    }

    public void setTaskAction(String action, String businessStatus, View.OnClickListener onClickListener) {
        actionTextView.setText(action);
        actionTextView.setOnClickListener(onClickListener);
    }
}
