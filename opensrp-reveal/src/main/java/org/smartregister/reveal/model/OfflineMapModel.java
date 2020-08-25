package org.smartregister.reveal.model;

import com.mapbox.mapboxsdk.offline.OfflineRegion;

import org.smartregister.domain.Location;

import java.util.Date;

public class OfflineMapModel {

    private Location location;

    private OfflineRegion offlineRegion;

    private Date dateCreated;

    public enum OfflineMapStatus {
        READY,
        DOWNLOAD_STARTED,
        DOWNLOADED,
        SELECTED_FOR_DOWNLOAD
    }

    private OfflineMapStatus offlineMapStatus;

    public OfflineMapModel() {
        setOfflineMapStatus(OfflineMapStatus.READY);
    }

    public Location getLocation() {
        return location;
    }

    public void setLocation(Location location) {
        this.location = location;
    }

    public OfflineMapStatus getOfflineMapStatus() {
        return offlineMapStatus;
    }

    public void setOfflineMapStatus(OfflineMapStatus offlineMapStatus) {
        this.offlineMapStatus = offlineMapStatus;
    }

    public String getDownloadAreaLabel() {
        return  (location != null && location.getProperties() != null) ? location.getProperties().getName() : null;
    }

    public String getDownloadAreaId() {
        return location != null ? location.getId() : null;
    }

    public OfflineRegion getOfflineRegion() {
        return offlineRegion;
    }

    public void setOfflineRegion(OfflineRegion offlineRegion) {
        this.offlineRegion = offlineRegion;
    }

    public Date getDateCreated() {
        return dateCreated;
    }

    public void setDateCreated(Date dateCreated) {
        this.dateCreated = dateCreated;
    }
}
