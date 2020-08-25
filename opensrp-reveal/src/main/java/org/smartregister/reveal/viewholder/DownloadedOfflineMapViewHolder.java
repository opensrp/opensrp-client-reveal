package org.smartregister.reveal.viewholder;

import androidx.annotation.NonNull;
import android.view.View;
import android.widget.TextView;

import org.smartregister.reveal.R;

public class DownloadedOfflineMapViewHolder extends BaseOfflineMapViewHolder {

    private TextView tvDownloadingLabel;


    public DownloadedOfflineMapViewHolder(@NonNull View itemView) {
        super(itemView);
        tvDownloadingLabel = itemView.findViewById(R.id.downloading_label);
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

}
