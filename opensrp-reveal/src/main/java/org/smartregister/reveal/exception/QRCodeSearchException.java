package org.smartregister.reveal.exception;

public class QRCodeSearchException extends Exception {
    private final String searchMessage;
    private final String qrCode;
    public QRCodeSearchException(String qrCode, String searchMessage) {
        super(searchMessage);
        this.qrCode = qrCode;
        this.searchMessage = searchMessage;
    }

    public String getQrCode() {
        return qrCode;
    }

    public String getSearchMessage() {
        return searchMessage;
    }
}
