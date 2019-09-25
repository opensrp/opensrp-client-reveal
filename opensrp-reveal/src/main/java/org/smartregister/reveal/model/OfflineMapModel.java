package org.smartregister.reveal.model;

public class OfflineMapModel {

    private String downloadAreaLabel;

    private boolean downloaded;

    public String getDownloadAreaLabel() {
        return downloadAreaLabel;
    }

    public void setDownloadAreaLabel(String downloadAreaLabel) {
        this.downloadAreaLabel = downloadAreaLabel;
    }

    public boolean isDownloaded() {
        return downloaded;
    }

    public void setDownloaded(boolean downloaded) {
        this.downloaded = downloaded;
    }
}
