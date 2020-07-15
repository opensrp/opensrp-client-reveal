package org.smartregister.reveal.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.Constants;


public class FamilyStructureTaskAdapter extends AbstractStructureTaskAdapter<FamilyStructureTaskAdapter.FamilyStructureViewHolder> {

    private View.OnClickListener onClickListener;

    public FamilyStructureTaskAdapter(View.OnClickListener onClickListener) {
        this.onClickListener = onClickListener;
    }

    @NonNull
    @Override
    public FamilyStructureViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.child_register_list_row, parent, false);
        return new FamilyStructureViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull FamilyStructureViewHolder holder, int position) {
        StructureTaskDetails taskDetails = taskDetailsList.get(position);
        holder.resetView();
        holder.bindView(taskDetails, onClickListener);
    }


    protected static class FamilyStructureViewHolder extends RecyclerView.ViewHolder {

        private TextView tvName;
        private TextView tvDetails;
        private View currentView;
        private View linearLayoutAction;
        private TextView tvHeader;
        private Button btnDue;
        private Context context;

        private FamilyStructureViewHolder(@NonNull View itemView) {
            super(itemView);
            currentView = itemView;
            tvHeader = itemView.findViewById(R.id.tvHeader);
            tvName = itemView.findViewById(R.id.patient_name);
            tvDetails = itemView.findViewById(R.id.tvDetails);
            linearLayoutAction = itemView.findViewById(R.id.linearLayoutAction);
            btnDue = itemView.findViewById(R.id.btnDue);
            context = itemView.getContext();
        }

        public void bindView(StructureTaskDetails details, View.OnClickListener onClickListener) {
            tvName.setText(details.getFamilyMemberNames());
            currentView.setOnClickListener(onClickListener);

            if (StringUtils.isBlank(details.getTaskStatus()))
                return;

            switch (details.getBusinessStatus()) {
                case Constants.BusinessStatus.NOT_VISITED:

                    btnDue.setTag(R.id.task_details, details);
                    btnDue.setOnClickListener(onClickListener);
                    linearLayoutAction.setOnClickListener(null);
                    linearLayoutAction.setVisibility(View.VISIBLE);

                    btnDue.setText(context.getResources().getString(R.string.record_dose));
                    tvDetails.setText("Dispense Dose");
                    btnDue.setTextColor(context.getResources().getColor(R.color.mda_partially_received));
                    btnDue.setBackground(context.getResources().getDrawable(R.drawable.due_dose_bg));
                    break;
                case Constants.BusinessStatus.VISITED_DRUG_ADMINISTERED:
                    btnDue.setOnClickListener(null);
                    linearLayoutAction.setOnClickListener(null);
                    linearLayoutAction.setVisibility(View.VISIBLE);

                    btnDue.setText(context.getResources().getString(R.string.dose_given));
                    btnDue.setTextColor(context.getResources().getColor(R.color.alert_complete_green));
                    btnDue.setBackground(null);
                    break;
                case Constants.BusinessStatus.VISITED_DRUG_NOT_ADMINISTERED:
                    btnDue.setOnClickListener(null);
                    linearLayoutAction.setOnClickListener(null);
                    linearLayoutAction.setVisibility(View.VISIBLE);

                    btnDue.setText(context.getResources().getString(R.string.not_given));
                    btnDue.setTextColor(context.getResources().getColor(R.color.dark_grey));
                    btnDue.setBackground(null);
                    break;
                default:
                    btnDue.setOnClickListener(null);
                    linearLayoutAction.setOnClickListener(null);
                    linearLayoutAction.setVisibility(View.GONE);
                    break;
            }

        }

        public void resetView() {
            tvHeader.setText("");
            tvHeader.setVisibility(View.GONE);

            tvName.setText("");
            tvDetails.setText("");
            linearLayoutAction.setVisibility(View.GONE);
            linearLayoutAction.setOnClickListener(null);
            btnDue.setBackground(null);
        }

    }
}
