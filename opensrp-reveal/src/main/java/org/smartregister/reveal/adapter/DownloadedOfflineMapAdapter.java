package org.smartregister.reveal.adapter;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import android.text.format.Formatter;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.mapbox.mapboxsdk.offline.OfflineRegion;
import com.mapbox.mapboxsdk.offline.OfflineRegionStatus;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.util.Utils;
import org.smartregister.reveal.viewholder.DownloadedOfflineMapViewHolder;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import static com.mapbox.mapboxsdk.offline.OfflineRegion.STATE_ACTIVE;
import static org.smartregister.reveal.model.OfflineMapModel.OfflineMapStatus.DOWNLOAD_STARTED;

public class DownloadedOfflineMapAdapter extends RecyclerView.Adapter<DownloadedOfflineMapViewHolder> {

    private Context context;

    private View.OnClickListener offlineMapClickHandler;
    private int selectedIndex = -1;

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
        viewHolder.setItemViewListener(offlineMapModel, (view) -> {
            int oldValue = selectedIndex;
            selectedIndex = position;
            if(selectedIndex == oldValue) {
                selectedIndex = -1;
            }
            offlineMapClickHandler.onClick(view);
            if (oldValue >= 0) {
                notifyItemChanged(oldValue);
            }
        });
        if (selectedIndex == position) {
            viewHolder.checkCheckBox(true);
        } else {
            switch (offlineMapModel.getOfflineMapStatus()) {
                case READY:
                case DOWNLOADED:
                    viewHolder.checkCheckBox(false);
                    break;
                default:
                    break;

            }
        }

        displayOfflineMapSizeAndStatus(offlineMapModel, viewHolder);

    }

    private void displayOfflineMapSizeAndStatus(OfflineMapModel offlineMapModel, DownloadedOfflineMapViewHolder viewHolder) {
        if (offlineMapModel == null || offlineMapModel.getOfflineRegion() == null) {
            return;
        }

        offlineMapModel.getOfflineRegion().getStatus(new OfflineRegion.OfflineRegionStatusCallback() {
            @Override
            public void onStatus(OfflineRegionStatus status) {

                viewHolder.displayDownloadSizeLabel(true);

                String mapDownloadSize = Formatter.formatFileSize(context, status.getCompletedResourceSize());
                Date dateCreated = offlineMapModel.getDateCreated() != null ? offlineMapModel.getDateCreated() : new Date();
                String downloadDate = Utils.formatDate(dateCreated);
                offlineMapModel.isPending = !status.isComplete();
                viewHolder.setDownloadedMapSize(context.getString(R.string.offline_map_size, mapDownloadSize, downloadDate));
                if(status.getDownloadState() == STATE_ACTIVE) {
                    offlineMapModel.setOfflineMapStatus(DOWNLOAD_STARTED);
                    viewHolder.displayDownloading();
                    viewHolder.checkCheckBox(true);
                    viewHolder.enableCheckBox(false);
                } else if (offlineMapModel.isPending) {
                    viewHolder.displayIncomplete();
                } else {
                    viewHolder.displaySuccess();
                }
            }

            @Override
            public void onError(String error) {
                // Do nothing
            }
        });

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
