package org.smartregister.reveal.adapter;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.viewholder.DownloadedOfflineMapViewHolder;

import java.util.ArrayList;
import java.util.List;

public class DownloadedOfflineMapAdapter extends RecyclerView.Adapter<DownloadedOfflineMapViewHolder> {

    private Context context;

    private View.OnClickListener offlineMapClickHandler;

    private List<OfflineMapModel> offlineMapModels = new ArrayList<>();

    public DownloadedOfflineMapAdapter(Context context, View.OnClickListener offlineMapClickHandler) {
        this.context = context;
        this.offlineMapClickHandler = offlineMapClickHandler;

    }

    @NonNull
    @Override
    public DownloadedOfflineMapViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int i) {
        View view = LayoutInflater.from(context).inflate(R.layout.offline_map_row, parent, false);
        return new DownloadedOfflineMapViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull DownloadedOfflineMapViewHolder viewHolder, int position) {
        OfflineMapModel offlineMapModel = offlineMapModels.get(position);
        viewHolder.setOfflineMapLabel(offlineMapModel.getDownloadAreaLabel());
        viewHolder.setItemViewListener(offlineMapModel, offlineMapClickHandler);

    }

    @Override
    public int getItemCount() {
        return offlineMapModels.size();
    }

    public void setOfflineMapModels(List<OfflineMapModel> offlineMapModels) {
        this.offlineMapModels = offlineMapModels;
        notifyDataSetChanged();
    }
}