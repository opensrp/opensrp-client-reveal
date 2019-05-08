package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.widget.TextView;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.CardDetailsUtil;
import org.smartregister.reveal.util.Constants;

/**
 * Created by samuelgithengi on 4/11/19.
 */
public class StructureTaskViewHolder extends RecyclerView.ViewHolder {

    private Context context;

    private TextView nameTextView;

    private TextView actionTextView;

    public StructureTaskViewHolder(@NonNull View itemView) {
        super(itemView);
        context = itemView.getContext();
        nameTextView = itemView.findViewById(R.id.task_name);
        actionTextView = itemView.findViewById(R.id.task_action);
    }

    public void setTaskName(String name) {
        nameTextView.setText(name);
    }

    public void setTaskAction(StructureTaskDetails taskDetails, View.OnClickListener onClickListener) {
        if (!Constants.BusinessStatus.NOT_VISITED.equals(taskDetails.getBusinessStatus())) {
            actionTextView.setText(taskDetails.getBusinessStatus());
            actionTextView.setBackground(null);
            CardDetails cardDetails = new CardDetails(taskDetails.getBusinessStatus());
            CardDetailsUtil.formatCardDetails(cardDetails);
            actionTextView.setTextColor(context.getResources().getColor(cardDetails.getStatusColor()));
        } else {
            actionTextView.setText(taskDetails.getTaskAction());
            actionTextView.setBackground(context.getResources().getDrawable(R.drawable.structure_task_action_bg));
            actionTextView.setTextColor(context.getResources().getColor(R.color.task_not_done));
        }
        actionTextView.setOnClickListener(onClickListener);
        actionTextView.setTag(R.id.task_details, taskDetails);
    }
}
