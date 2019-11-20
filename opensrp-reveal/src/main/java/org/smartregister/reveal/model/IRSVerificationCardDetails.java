package org.smartregister.reveal.model;

public class IRSVerificationCardDetails extends CardDetails {

    private String reportedSprayStatus;
    private String chalkSprayStatus;
    private String stickerSprayStatus;
    private String cardSprayStatus;

    public IRSVerificationCardDetails(String status, String reportedSS, String chalkSS, String stickerSS, String cardSS) {
        super(status);
        this.reportedSprayStatus = reportedSS;
        this.chalkSprayStatus = chalkSS;
        this.stickerSprayStatus = stickerSS;
        this.cardSprayStatus = cardSS;
    }

    public String getReportedSprayStatus() {
        return reportedSprayStatus;
    }

    public void setReportedSprayStatus(String reportedSprayStatus) {
        this.reportedSprayStatus = reportedSprayStatus;
    }

    public String getChalkSprayStatus() {
        return chalkSprayStatus;
    }

    public void setChalkSprayStatus(String chalkSprayStatus) {
        this.chalkSprayStatus = chalkSprayStatus;
    }

    public String getStickerSprayStatus() {
        return stickerSprayStatus;
    }

    public void setStickerSprayStatus(String stickerSprayStatus) {
        this.stickerSprayStatus = stickerSprayStatus;
    }

    public String getCardSprayStatus() {
        return cardSprayStatus;
    }

    public void setCardSprayStatus(String cardSprayStatus) {
        this.cardSprayStatus = cardSprayStatus;
    }
}
