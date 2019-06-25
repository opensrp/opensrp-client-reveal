package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.support.annotation.DrawableRes;
import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;


/**
 * Created by samuelgithengi on 3/12/19.
 */
public class TaskRegisterViewHolder extends RecyclerView.ViewHolder {

    private Context context;

    private ImageView iconView;

    private TextView nameView;

    private TextView distanceView;

    private TextView actionView;

    private TextView taskDetailsView;

    private PreferencesUtil prefsUtil = PreferencesUtil.getInstance();

    public TaskRegisterViewHolder(@NonNull View itemView) {
        super(itemView);
        context = itemView.getContext();
        iconView = itemView.findViewById(R.id.task_icon);
        nameView = itemView.findViewById(R.id.task_name);
        distanceView = itemView.findViewById(R.id.distance_from_structure);
        taskDetailsView = itemView.findViewById(R.id.task_details);
        actionView = itemView.findViewById(R.id.task_action);
    }


    public void setIcon(@DrawableRes int iconResource) {
        iconView.setVisibility(View.VISIBLE);
        iconView.setImageDrawable(context.getResources().getDrawable(iconResource));
    }

    public void setTaskName(String taskName) {
        nameView.setText(taskName);
    }

    public void setDistanceFromStructure(float distance, boolean distanceFromCenter) {
        if (distanceFromCenter) {
            distanceView.setText(context.getString(
                    R.string.distance_from_center, distance, prefsUtil.getCurrentOperationalArea()));
        } else {
            distanceView.setText(context.getString(R.string.distance_from_structure, distance));
        }
    }

    public void hideDistanceFromStructure() {
        distanceView.setVisibility(View.GONE);
    }


    /**
     * Method that handles the populating of action view information on each row of the
     * task register.
     *
     * It handles the text and color displayed on an Action View. Also attaches a clickListener
     * which handles clicks on the action view.
     *
     * @param actionLabel Text that shows what action to take when action view is clicked
     * @param task TaskDetails object used to populate info in a particular row
     * @param cardDetails Object that contains status, status message and status color
     * @param onClickListener Click listener that handles clicks events on the Task Action
     */
    public void setTaskAction(String actionLabel, TaskDetails task, CardDetails cardDetails, View.OnClickListener onClickListener) {
        actionView.setText(actionLabel);

        // registered family with multiple tasks
        if (cardDetails != null && task.getTaskCount() != null && Utils.getInterventionLabel() == R.string.focus_investigation) { // task grouping only for FI
            if (task.getTaskCount() > 1 ) {
                if (task.getTaskCount() != task.getCompleteTaskCount()) {
                    actionView.setBackground(context.getResources().getDrawable(R.drawable.view_tasks_bg));
                    actionView.setTextColor(context.getResources().getColor(R.color.text_black));
                    //choose different color coding only applicable to Thailand at the moment
                    if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                        if (task.isFamilyRegistered() && !task.isBednetDistributed() && !task.isAllBloodScreeningDone()) {
                            actionView.setBackground(context.getResources().getDrawable(R.drawable.family_registered_bg));
                            actionView.setTextColor(context.getResources().getColor(R.color.text_black));
                        } else if (task.isBednetDistributed() && task.isFamilyRegistered()) {
                            actionView.setBackground(context.getResources().getDrawable(R.drawable.bednet_distributed_bg));
                            actionView.setTextColor(context.getResources().getColor(R.color.text_black));
                        } else if (task.isAllBloodScreeningDone()) {
                            actionView.setBackground(context.getResources().getDrawable(R.drawable.blood_screening_complete_bg));
                            actionView.setTextColor(context.getResources().getColor(R.color.text_black));
                        } else if (task.getCompleteTaskCount() == 0) {
                            actionView.setBackground(context.getResources().getDrawable(R.drawable.blood_screening_complete_bg));
                            actionView.setTextColor(context.getResources().getColor(R.color.text_black));
                        } else {
                            actionView.setBackground(context.getResources().getDrawable(R.drawable.no_task_complete_bg));
                            actionView.setTextColor(context.getResources().getColor(R.color.text_black));
                        }
                    }

                    actionView.setText(context.getText(R.string.view_tasks));
                } else if (task.getTaskCount() == task.getCompleteTaskCount()){
                    actionView.setBackground(context.getResources().getDrawable(R.drawable.tasks_complete_bg));
                    actionView.setTextColor(context.getResources().getColor(R.color.text_black));
                    actionView.setText(context.getText(R.string.tasks_complete));
                }
            }

        } else if (cardDetails != null && cardDetails.getStatusColor() != null) {
            actionView.setBackground(null);
            actionView.setTextColor(context.getResources().getColor(cardDetails.getStatusColor()));
        } else {
            actionView.setBackground(context.getResources().getDrawable(R.drawable.task_action_bg));
            actionView.setTextColor(context.getResources().getColor(R.color.text_black));
        }
        actionView.setOnClickListener(onClickListener);
        actionView.setTag(R.id.task_details, task);
    }

    public void setTaskDetails(String businessStatus, String taskDetails) {
        if (Constants.BusinessStatus.NOT_SPRAYED.equals(businessStatus)) {
            taskDetailsView.setVisibility(View.VISIBLE);
            taskDetailsView.setText(context.getString(R.string.task_reason, taskDetails));
        } else {
            taskDetailsView.setVisibility(View.GONE);
        }
    }

    public void hideIcon() {
        iconView.setVisibility(View.GONE);
    }
}
