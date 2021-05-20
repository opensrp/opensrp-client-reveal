package org.smartregister.reveal.widget;

import android.widget.TextView;

import com.vijay.jsonwizard.widgets.RepeatingGroupFactory;

import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.activity.RevealJsonFormActivity;
import org.smartregister.reveal.util.Country;

import static org.smartregister.reveal.util.Constants.JsonForm.ROOMS_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.JsonForm.ROOMS_SPRAYED;
import static org.smartregister.util.JsonFormUtils.STEP1;

public class RevealRepeatingGroupFactory extends RepeatingGroupFactory {

    public static final String CONFIRMED_ROOMS_NOT_SPRAYED = "Confirmed # rooms not sprayed";
    public static final String CONFIRMED_ROOMS_NOT_SPRAYED_ERROR = "Please correct number of rooms not sprayed ";
    public static final String PLEASE_ENTER_A_VALUE_ERROR_MESSAGE = "Please enter a value";
    public static final String NUMBER_OF_INSECTICIDE_SACHETS_MIXED = "Number of insecticide sachets mixed";
    public static final String NUMBER_OF_INSECTICIDE_SACHETS_VALIDATION_ERR_MESSAGE = "number of insecticide sachets must be greater than 0";

    @Override
    protected void addOnDoneAction(TextView textView) {
        if(Country.SENEGAL.equals(BuildConfig.BUILD_COUNTRY)){
            String inputText = textView.getText().toString();
            if(inputText.isEmpty()){
                textView.setError(PLEASE_ENTER_A_VALUE_ERROR_MESSAGE);
                return;
            }
            RevealJsonFormActivity activity = (RevealJsonFormActivity) textView.getContext();
            Integer reasonsOrSachetCount = Integer.parseInt(inputText);
            if(CONFIRMED_ROOMS_NOT_SPRAYED.equals(textView.getHint().toString())){
                TextView roomSprayedTextView = (TextView) activity.getFormDataView(  STEP1 + ":" + ROOMS_SPRAYED);
                Integer roomsSprayedCount = Integer.parseInt(roomSprayedTextView.getText().toString());
                TextView roomsEligibleTextView = (TextView) activity.getFormDataView(STEP1 + ":" + ROOMS_ELIGIBLE);
                Integer roomsEligible = Integer.parseInt(roomsEligibleTextView.getText().toString());
                if (reasonsOrSachetCount != (roomsEligible - roomsSprayedCount)){
                    textView.setError(CONFIRMED_ROOMS_NOT_SPRAYED_ERROR);
                    return;
                }
            } else if(NUMBER_OF_INSECTICIDE_SACHETS_MIXED.equals(textView.getHint().toString())) {
                if(reasonsOrSachetCount  < 1) {
                    textView.setError(NUMBER_OF_INSECTICIDE_SACHETS_VALIDATION_ERR_MESSAGE);
                    return;
                }
            }

        }
        super.addOnDoneAction(textView);
    }
}
