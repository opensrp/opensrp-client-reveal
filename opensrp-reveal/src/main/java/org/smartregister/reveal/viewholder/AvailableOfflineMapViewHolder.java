package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.widget.TextView;

import org.smartregister.reveal.R;

public class AvailableOfflineMapViewHolder extends RecyclerView.ViewHolder {

    private Context context;

    private TextView offlineMapTextView;

    public AvailableOfflineMapViewHolder(@NonNull View itemView) {
        super(itemView);
        context = itemView.getContext();
        offlineMapTextView = itemView.findViewById(R.id.offline_map_label);
    }

    public void setOfflineMapLabel(String offlineMapLabel) {
        this.offlineMapTextView.setText(offlineMapLabel);
    }
}
