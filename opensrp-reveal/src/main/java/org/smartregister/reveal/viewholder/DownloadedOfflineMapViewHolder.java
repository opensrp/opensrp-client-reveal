package org.smartregister.reveal.viewholder;

import androidx.annotation.NonNull;
import androidx.annotation.StringRes;
import androidx.core.content.ContextCompat;

import android.view.View;
import android.widget.TextView;

import org.smartregister.reveal.R;

public class DownloadedOfflineMapViewHolder extends BaseOfflineMapViewHolder {

    private final TextView tvDownloadingLabel;
    private final TextView tvMapStatusLabel;


    public DownloadedOfflineMapViewHolder(@NonNull View itemView) {
        super(itemView);
        tvDownloadingLabel = itemView.findViewById(R.id.downloading_label);
        tvMapStatusLabel = itemView.findViewById(R.id.map_status_label);
    }

    public void setDownloadedMapSize(String offlineMapSize) {
        this.tvDownloadingLabel.setText(offlineMapSize);
    }


    public void displayDownloadSizeLabel(boolean displayDownloadSizeLabel) {
        if (displayDownloadSizeLabel) {
            tvDownloadingLabel.setVisibility(View.VISIBLE);
        } else {
            tvDownloadingLabel.setVisibility(View.GONE);
        }
    }

    public void displaySuccess() {
        displayStatus(R.string.complete);
    }

    public void displayIncomplete() {
        tvMapStatusLabel.setTextColor(ContextCompat.getColor(itemView.getContext(), R.color.alert_urgent_red));
        displayStatus(R.string.incomplete);
    }

    private void displayStatus(@StringRes int resId){
        tvMapStatusLabel.setText(resId);
        tvMapStatusLabel.setVisibility(View.VISIBLE);
    }
}
