package org.smartregister.reveal.model;

import org.smartregister.domain.Location;

public class OfflineMapModel {

    private Location location;

    public enum OFFLINE_MAP_STATUS {
        READY,
        DOWNLOAD_STARTED,
        DOWNLOADED
    }

    public OfflineMapModel() {
        setOfflineMapStatus(OFFLINE_MAP_STATUS.READY);
    }

    private OFFLINE_MAP_STATUS offlineMapStatus;

    public Location getLocation() {
        return location;
    }

    public void setLocation(Location location) {
        this.location = location;
    }

    public OFFLINE_MAP_STATUS getOfflineMapStatus() {
        return offlineMapStatus;
    }

    public void setOfflineMapStatus(OFFLINE_MAP_STATUS offlineMapStatus) {
        this.offlineMapStatus = offlineMapStatus;
    }

    public String getDownloadAreaLabel() {
        return  (location != null && location.getProperties() != null) ? location.getProperties().getName() : null;
    }

    public String getDownloadAreaId() {
        return location != null ? location.getId() : null;
    }
}
