package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.widget.CheckBox;
import android.widget.TextView;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.OfflineMapModel;

public class AvailableOfflineMapViewHolder extends RecyclerView.ViewHolder {

    private Context context;

    private TextView offlineMapTextView;

    private CheckBox offlineMapCheckBox;

    public AvailableOfflineMapViewHolder(@NonNull View itemView) {
        super(itemView);
        context = itemView.getContext();
        offlineMapTextView = itemView.findViewById(R.id.offline_map_label);
        offlineMapCheckBox = itemView.findViewById(R.id.offline_map_checkbox);
    }

    public void setOfflineMapLabel(String offlineMapLabel) {
        this.offlineMapTextView.setText(offlineMapLabel);
    }

    public void setItemViewListener(OfflineMapModel offlineMapModel, View.OnClickListener onClickListener) {
        offlineMapCheckBox.setOnClickListener(onClickListener);
        offlineMapCheckBox.setTag(R.id.offline_map_checkbox, offlineMapModel);
    }
}
