package org.smartregister.reveal.model;

public class MDAOutCome {
    private int expectedForms = 0;
    private int negativeForms = 0;
    private int positiveForms = 0;

    public void setExpectedForms(int expectedForms) {
        this.expectedForms = expectedForms;
    }

    public void setNegativeForms(int negativeForms) {
        this.negativeForms = negativeForms;
    }

    public void setPositiveForms(int positiveForms) {
        this.positiveForms = positiveForms;
    }

    private int completedForms(){
        return negativeForms + positiveForms;
    }

    public MDAOutComeStatus getStatus(){
        int completedForms = completedForms();

        if(completedForms > 0 || expectedForms > 0){

            if(completedForms == 0 || (negativeForms == completedForms && negativeForms >= expectedForms)){
                return MDAOutComeStatus.NEGATIVE;
            }else if(expectedForms > completedForms || (expectedForms >= completedForms && positiveForms > 0)){
                return MDAOutComeStatus.PARTIAL;
            }
        }

        return MDAOutComeStatus.POSITIVE;
    }

    public enum MDAOutComeStatus {
        POSITIVE , PARTIAL , NEGATIVE
    }
}
