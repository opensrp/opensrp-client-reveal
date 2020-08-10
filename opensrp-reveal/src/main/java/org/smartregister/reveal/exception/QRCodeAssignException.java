package org.smartregister.reveal.exception;

public class QRCodeAssignException extends Exception {
    private final String assignErrorMessage;
    private final String qrCode;

    public QRCodeAssignException(String qrCode, String assignErrorMessage) {
        super(assignErrorMessage);
        this.qrCode = qrCode;
        this.assignErrorMessage = assignErrorMessage;
    }

    public String getQrCode() {
        return qrCode;
    }

    public String getAssignErrorMessage() {
        return assignErrorMessage;
    }
}
