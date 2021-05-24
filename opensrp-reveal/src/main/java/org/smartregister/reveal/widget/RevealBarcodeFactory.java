package org.smartregister.reveal.widget;

import android.app.Activity;
import android.content.Context;
import android.view.View;
import android.widget.RadioButton;
import android.widget.RadioGroup;

import com.google.android.gms.vision.barcode.Barcode;
import com.rengwuxian.materialedittext.MaterialEditText;
import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.customviews.MaterialSpinner;
import com.vijay.jsonwizard.interfaces.JsonApi;
import com.vijay.jsonwizard.utils.Utils;
import com.vijay.jsonwizard.widgets.BarcodeFactory;

import org.joda.time.DateTime;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.domain.Event;
import org.smartregister.domain.Obs;
import org.smartregister.domain.db.EventClient;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.reveal.activity.RevealJsonFormActivity;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.FamilyConstants;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import timber.log.Timber;

import static android.app.Activity.RESULT_OK;
import static org.smartregister.family.util.Utils.metadata;
import static org.smartregister.util.JsonFormUtils.STEP1;

public class RevealBarcodeFactory extends BarcodeFactory {

    public static final String CHILD_FIRST_NAME = "childFirstName";
    public static final String SURNAME_OF_CHILD = "surnameOfChild";
    public static final String SEX = "sex";
    public static final String LAST_NAME = "last_name";
    public static final String GENDER = "gender";
    public static final String REFERRAL_QR_CODE = "referralQRCode";
    public static final String REFERRAL_QR_CODE_SEARCH = "Referral QR Code Search";
    public static final String OTHER = "Other";
    public static final Integer OPTION_COUNT = 7;

    @Override
    protected void launchBarcodeScanner(Activity activity, MaterialEditText editText, String barcodeType) {

        super.launchBarcodeScanner(activity, editText, barcodeType);
    }

    @Override
    protected void addOnBarCodeResultListeners(Context context, MaterialEditText editText) {
        if (context instanceof JsonApi) {
            JsonApi jsonApi = (JsonApi) context;
            jsonApi.addOnActivityResultListener(JsonFormConstants.BARCODE_CONSTANTS.BARCODE_REQUEST_CODE,
                    (requestCode, resultCode, data) -> {
                        if (requestCode == JsonFormConstants.BARCODE_CONSTANTS.BARCODE_REQUEST_CODE && resultCode == RESULT_OK) {
                            if (data != null) {
                                Barcode barcode = data.getParcelableExtra(JsonFormConstants.BARCODE_CONSTANTS.BARCODE_KEY);
                                Timber.d("Scanned QR Code %s ", barcode.displayValue);
                                editText.setText(barcode.displayValue);
                                if(editText.getFloatingLabelText().equals(REFERRAL_QR_CODE_SEARCH)){
                                     searchForChildAndUpdateForm(barcode.displayValue, context);
                                }
                            } else
                                Timber.i("NO RESULT FOR QR CODE");
                        }
                    });
        }
    }
    private void searchForChildAndUpdateForm(String qrCode, Context context){
        String childID = null;
        DateTime referralDate = null;
        String referredHF = null;
        String referralReasons = null;
        String otherReason = null;
        EventClientRepository eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
        List<EventClient> eventClients =  eventClientRepository.fetchEventClientsByEventTypes(Arrays.asList(Constants.EventType.MDA_DISPENSE,Constants.EventType.MDA_ADHERENCE));
        List<Event> referralEvents = eventClients.stream()
                                                 .map(eventClient -> eventClient.getEvent())
                                                 .filter(event -> (event.getObs().stream().filter(obs -> obs.getFieldCode().equals(REFERRAL_QR_CODE)).findFirst().isPresent() && event.getDateVoided() == null)).collect(Collectors.toList());
        for (Event referral : referralEvents) {
            Optional<Obs> optional = referral.getObs().stream().filter(obs -> obs.getFieldCode().equals(REFERRAL_QR_CODE)).findFirst();
            if (optional.isPresent()) {
                Obs storedQRCodeObs = optional.get();
                if (qrCode.equals(storedQRCodeObs.getValue().toString())) {
                    childID = referral.getBaseEntityId();
                    referralDate = referral.getEventDate();
                    Obs referralReasonsObs;
                    Obs referredHFObs;
                    if(referral.getEventType().equals(Constants.EventType.MDA_ADHERENCE)){
                        referredHFObs = referral.getObs().stream().filter(obs -> obs.getFormSubmissionField().equals("child_referred_hf")).findFirst().get();
                        referralReasonsObs = referral.getObs().stream().filter(obs -> obs.getFormSubmissionField().equals("referralReason")).findFirst().get();
                    } else {
                        referredHFObs = referral.getObs().stream().filter(obs -> obs.getFormSubmissionField().equals("referredHf")).findFirst().get();
                        referralReasonsObs = referral.getObs().stream().filter(obs -> obs.getFormSubmissionField().equals("referralReasons")).findFirst().get();
                    }
                    referralReasons  =  referralReasonsObs.getValue().toString();
                    if(referralReasons.startsWith(OTHER)){
                        otherReason = referral.getObs().stream().filter(obs -> obs.getFormSubmissionField().equals("otherReason")).findFirst().get().getValue().toString();
                    }
                    referredHF = referredHFObs.getValue().toString();
                    break;
                }
            }
        }

        RevealJsonFormActivity activity = (RevealJsonFormActivity) context;
        MaterialEditText firstNameTextField = (MaterialEditText) activity.getFormDataView(STEP1 + ":" + CHILD_FIRST_NAME);
        MaterialEditText lastNameTextField = (MaterialEditText) activity.getFormDataView(STEP1 + ":" + SURNAME_OF_CHILD);
        RadioGroup radioGroup = (RadioGroup) activity.getFormDataView(STEP1 + ":" + SEX);
        List<RadioButton> radioButtons = Utils.getRadioButtons(radioGroup);
        MaterialEditText referralDateField = (MaterialEditText) activity.getFormDataView(STEP1 + ":" + "dateOfReferral");
        MaterialEditText healthFacilityField = (MaterialEditText) activity.getFormDataView(STEP1 + ":" + "health_facility");
        MaterialEditText otherReferralReasonField  = (MaterialEditText) activity.getFormDataView(STEP1 + ":" + "otherRefferalReason");
        MaterialSpinner referralReasonSpinner = (MaterialSpinner) activity.getFormDataView(STEP1 + ":" + "referralReason");


        for(int i=0;i < OPTION_COUNT ;i++){
            String option = referralReasonSpinner.getItemAtPosition(i).toString();
            if(option.equals(referralReasons)){
                referralReasonSpinner.setSelection(i + 1);
                referralReasonSpinner.setEnabled(false);
                break;
            }
        }


        RadioGroup birthDateUnknownRadioGroup = (RadioGroup) activity.getFormDataView(STEP1 + ":" + "birthdate_unknown");
        List<RadioButton> birthDateUnknownRadioButtons = Utils.getRadioButtons(birthDateUnknownRadioGroup);
        birthDateUnknownRadioButtons.stream().forEach(radioButton -> {
            radioButton.setEnabled(false);
            if("Yes".equals(radioButton.getText().toString()))
                radioButton.setChecked(true);

        });
     MaterialEditText birthDateField = (MaterialEditText) activity.getFormDataView(STEP1 + ":" + "dob");




        if(childID != null){
            referralDateField.setText(referralDate.toString());
            referralDateField.setEnabled(false);
            healthFacilityField.setText(referredHF);
            healthFacilityField.setEnabled(false);
            CommonRepository commonRepository  = RevealApplication.getInstance().getContext().commonrepository(metadata().familyMemberRegister.tableName);
            CommonPersonObject child = commonRepository.findByBaseEntityId(childID);
            Map<String,String> childDetails = child.getColumnmaps();
            String firstName = childDetails.get(FamilyConstants.FormKeys.FIRST_NAME);
            String lastName = childDetails.get(LAST_NAME);
            String gender = childDetails.get(GENDER);
            String dob = childDetails.get("dob");

            firstNameTextField.setText(firstName);
            firstNameTextField.setEnabled(false);
            lastNameTextField.setText(lastName);
            lastNameTextField.setEnabled(false);
            radioButtons.stream().forEach(radioButton -> {
                radioButton.setEnabled(false);
                if(gender.equalsIgnoreCase(radioButton.getText().toString()))
                   radioButton.setChecked(true);
            });
            birthDateField.setText(dob);
            birthDateField.setEnabled(false);
            if(referralReasons.startsWith(OTHER)){
                otherReferralReasonField.setText(otherReason);
                otherReferralReasonField.setEnabled(false);
            }
        }else {
            referralDateField.setText("");
            referralDateField.setEnabled(true);
            healthFacilityField.setText("");
            healthFacilityField.setEnabled(true);
            birthDateField.setText("");
            birthDateField.setEnabled(true);
            firstNameTextField.setEnabled(true);
            firstNameTextField.setText("");
            lastNameTextField.setEnabled(true);
            lastNameTextField.setText("");
            otherReferralReasonField.setText("");
            otherReferralReasonField.setEnabled(true);
            radioButtons.stream().forEach(radioButton -> {
                radioButton.setChecked(false);
                radioButton.setEnabled(true);
            });
            birthDateUnknownRadioButtons.stream().forEach(radioButton -> {
                radioButton.setEnabled(true);
                radioButton.setEnabled(true);
            });
        }
    }

}
