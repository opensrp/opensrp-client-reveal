package org.smartregister.reveal.viewholder;

import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.widget.CheckBox;
import android.widget.TextView;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.OfflineMapModel;

public class DownloadedOfflineMapViewHolder extends RecyclerView.ViewHolder {

    private TextView tvOfflineMapNameLabel;
    private CheckBox offlineMapCheckBox;


    public DownloadedOfflineMapViewHolder(@NonNull View itemView) {
        super(itemView);

        tvOfflineMapNameLabel = itemView.findViewById(R.id.offline_map_label);
        offlineMapCheckBox = itemView.findViewById(R.id.offline_map_checkbox);
    }

    public void setOfflineMapLabel(String offlineMapLabel) {
        this.tvOfflineMapNameLabel.setText(offlineMapLabel);
    }

    public void setItemViewListener(OfflineMapModel offlineMapModel, View.OnClickListener onClickListener) {
        offlineMapCheckBox.setOnClickListener(onClickListener);
        offlineMapCheckBox.setTag(R.id.offline_map_checkbox, offlineMapModel);
    }

    public void checkCheckBox(boolean check) {
        offlineMapCheckBox.setChecked(check);
    }

}
