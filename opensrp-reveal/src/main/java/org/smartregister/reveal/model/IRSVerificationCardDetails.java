package org.smartregister.reveal.model;

public class IRSVerificationCardDetails extends CardDetails {

    private String reportedSpray;
    private String chalkSpray;
    private String stickerSpray;
    private String cardSpray;

    public IRSVerificationCardDetails(String status, String reportedSS, String chalkSS, String stickerSS, String cardSS) {
        super(status);
        this.reportedSpray = reportedSS;
        this.chalkSpray = chalkSS;
        this.stickerSpray = stickerSS;
        this.cardSpray = cardSS;
    }

    public String getReportedSpray() {
        return reportedSpray;
    }

    public void setReportedSpray(String reportedSpray) {
        this.reportedSpray = reportedSpray;
    }

    public String getChalkSpray() {
        return chalkSpray;
    }

    public void setChalkSpray(String chalkSpray) {
        this.chalkSpray = chalkSpray;
    }

    public String getStickerSpray() {
        return stickerSpray;
    }

    public void setStickerSpray(String stickerSpray) {
        this.stickerSpray = stickerSpray;
    }

    public String getCardSpray() {
        return cardSpray;
    }

    public void setCardSpray(String cardSpray) {
        this.cardSpray = cardSpray;
    }
}
