package org.smartregister.reveal.util;

import android.content.Context;
import android.text.format.Formatter;

import com.mapbox.mapboxsdk.offline.OfflineRegion;
import com.mapbox.mapboxsdk.offline.OfflineRegionStatus;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.viewholder.DownloadedOfflineMapViewHolder;

import java.util.Date;

import static com.mapbox.mapboxsdk.offline.OfflineRegion.STATE_ACTIVE;
import static org.smartregister.reveal.model.OfflineMapModel.OfflineMapStatus.DOWNLOAD_STARTED;

public class CustomOfflineRegionCallback implements OfflineRegion.OfflineRegionStatusCallback {

    private final DownloadedOfflineMapViewHolder viewHolder;
    private final OfflineMapModel offlineMapModel;

    public CustomOfflineRegionCallback(DownloadedOfflineMapViewHolder viewHolder, OfflineMapModel offlineMapModel) {
        this.viewHolder = viewHolder;
        this.offlineMapModel = offlineMapModel;
    }


    public void onStatus(OfflineRegionStatus status) {
        Context  context = viewHolder.itemView.getContext();
        viewHolder.displayDownloadSizeLabel(true);
        String mapDownloadSize = Formatter.formatFileSize(context, status.getCompletedResourceSize());
        Date dateCreated = offlineMapModel.getDateCreated() != null ? offlineMapModel.getDateCreated() : new Date();
        String downloadDate = Utils.formatDate(dateCreated);
        offlineMapModel.isPending = !status.isComplete();
        viewHolder.setDownloadedMapSize(context.getString(R.string.offline_map_size, mapDownloadSize, downloadDate));
        if (status.getDownloadState() == STATE_ACTIVE) {
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

}
