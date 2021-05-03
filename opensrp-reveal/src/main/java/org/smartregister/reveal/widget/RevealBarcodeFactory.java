package org.smartregister.reveal.widget;

import android.app.Activity;
import android.content.Context;
import android.view.ViewGroup;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.RelativeLayout;

import com.google.android.gms.vision.barcode.Barcode;
import com.rengwuxian.materialedittext.MaterialEditText;
import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.interfaces.JsonApi;
import com.vijay.jsonwizard.utils.Utils;
import com.vijay.jsonwizard.widgets.BarcodeFactory;

import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.domain.Event;
import org.smartregister.domain.Obs;
import org.smartregister.domain.db.EventClient;
import org.smartregister.family.util.JsonFormUtils;
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

public class RevealBarcodeFactory extends BarcodeFactory {

    public static final String CHILD_FIRST_NAME = "childFirstName";
    public static final String SURNAME_OF_CHILD = "surnameOfChild";
    public static final String SEX = "sex";
    public static final String LAST_NAME = "last_name";
    public static final String GENDER = "gender";
    public static final String REFERRAL_QR_CODE = "referralQRCode";
    public static final String REFERRAL_QR_CODE_SEARCH = "Referral QR Code Search";

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
        EventClientRepository eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
        List<EventClient> eventClients =  eventClientRepository.fetchEventClientsByEventTypes(Arrays.asList(Constants.EventType.MDA_DISPENSE,Constants.EventType.MDA_ADHERENCE));
        List<Event> referralEvents = eventClients.stream()
                                                 .map(eventClient -> eventClient.getEvent())
                                                 .filter(event -> event.getObs().stream().filter(obs -> obs.getFieldCode().equals(REFERRAL_QR_CODE)).findFirst().isPresent()).collect(Collectors.toList());
        for (Event referral : referralEvents) {
            Optional<Obs> optional = referral.getObs().stream().filter(obs -> obs.getFieldCode().equals(REFERRAL_QR_CODE)).findFirst();
            if (optional.isPresent()) {
                Obs storedQRCodeObs = optional.get();
                if (qrCode.equals(storedQRCodeObs.getValue().toString())) {
                    childID = referral.getBaseEntityId();
                    break;
                }
            }
        }

        RevealJsonFormActivity activity = (RevealJsonFormActivity) context;
        MaterialEditText firstNameTextField = (MaterialEditText) activity.getFormDataView(JsonFormConstants.STEP1 + ":" + CHILD_FIRST_NAME);
        MaterialEditText lastNameTextField = (MaterialEditText) activity.getFormDataView(JsonFormConstants.STEP1 + ":" + SURNAME_OF_CHILD);
        RadioGroup radioGroup = (RadioGroup) activity.getFormDataView(JsonFormConstants.STEP1 + ":" + SEX);
        List<RadioButton> radioButtons = Utils.getRadioButtons(radioGroup);


        if(childID != null){
            
            CommonRepository commonRepository  = RevealApplication.getInstance().getContext().commonrepository(metadata().familyMemberRegister.tableName);
            CommonPersonObject child = commonRepository.findByBaseEntityId(childID);
            Map<String,String> childDetails = child.getColumnmaps();
            String firstName = childDetails.get(FamilyConstants.FormKeys.FIRST_NAME);
            String lastName = childDetails.get(LAST_NAME);
            String gender = childDetails.get(GENDER);

            firstNameTextField.setText(firstName);
            firstNameTextField.setEnabled(false);
            lastNameTextField.setText(lastName);
            lastNameTextField.setEnabled(false);
            radioButtons.stream().forEach(radioButton -> {
                radioButton.setEnabled(false);
                if(gender.equalsIgnoreCase(radioButton.getText().toString()))
                   radioButton.setChecked(true);
            });

        }else {
            
            firstNameTextField.setEnabled(true);
            firstNameTextField.setText("");
            lastNameTextField.setEnabled(true);
            lastNameTextField.setText("");
            radioButtons.stream().forEach(radioButton -> {
                radioButton.setChecked(false);
                radioButton.setEnabled(true);
            });
        }
    }

}
