package org.smartregister.reveal.model;

import org.smartregister.domain.Location;

public class OfflineMapModel {

    private boolean downloaded;

    private Location location;

    private boolean downloadStarted;

    public boolean isDownloaded() {
        return downloaded;
    }

    public void setDownloaded(boolean downloaded) {
        this.downloaded = downloaded;
    }

    public Location getLocation() {
        return location;
    }

    public void setLocation(Location location) {
        this.location = location;
    }

    public boolean isDownloadStarted() {
        return downloadStarted;
    }

    public void setDownloadStarted(boolean downloadStarted) {
        this.downloadStarted = downloadStarted;
    }

    public String getDownloadAreaLabel() {
        return  (location != null && location.getProperties() != null) ? location.getProperties().getName() : null;
    }

    public String getDownloadAreaId() {
        return location != null ? location.getId() : null;
    }
}
