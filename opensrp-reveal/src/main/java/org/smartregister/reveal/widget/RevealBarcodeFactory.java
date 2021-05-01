package org.smartregister.reveal.widget;

import android.app.Activity;
import android.content.Context;

import com.google.android.gms.vision.barcode.Barcode;
import com.rengwuxian.materialedittext.MaterialEditText;
import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.interfaces.JsonApi;
import com.vijay.jsonwizard.widgets.BarcodeFactory;

import org.json.JSONException;
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

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.json.JsonObject;

import timber.log.Timber;

import static android.app.Activity.RESULT_OK;
import static org.smartregister.family.util.Utils.metadata;

public class RevealBarcodeFactory extends BarcodeFactory {
    @Override
    protected void launchBarcodeScanner(Activity activity, MaterialEditText editText, String barcodeType) {
        super.launchBarcodeScanner(activity, editText, barcodeType);
    }

    @Override
    protected void addOnBarCodeResultListeners(Context context, MaterialEditText editText) {
        System.out.println("Attached to RevealBarcodeFactory#addOnBarCodeResultListeners");
        if (context instanceof JsonApi) {
            JsonApi jsonApi = (JsonApi) context;
            jsonApi.addOnActivityResultListener(JsonFormConstants.BARCODE_CONSTANTS.BARCODE_REQUEST_CODE,
                    (requestCode, resultCode, data) -> {
                        if (requestCode == JsonFormConstants.BARCODE_CONSTANTS.BARCODE_REQUEST_CODE && resultCode == RESULT_OK) {
                            if (data != null) {
                                Barcode barcode = data.getParcelableExtra(JsonFormConstants.BARCODE_CONSTANTS.BARCODE_KEY);
                                Timber.d("Scanned QR Code %s ", barcode.displayValue);
                                editText.setText(barcode.displayValue);
                                if(editText.getFloatingLabelText().equals("Referral QR Code Search")){
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
                                                 .filter(event -> event.getObs().stream().filter(obs -> obs.getFieldCode().equals("referralQRCode")).findFirst().isPresent()).collect(Collectors.toList());
        for (Event referral : referralEvents) {
            Optional<Obs> optional = referral.getObs().stream().filter(obs -> obs.getFieldCode().equals("referralQRCode")).findFirst();
            if (optional.isPresent()) {
                Obs storedQRCodeObs = optional.get();
                if (qrCode.equals(storedQRCodeObs.getValue().toString())) {
                    childID = referral.getBaseEntityId();
                    break;
                }
            }
        }

        if(childID != null){
            CommonRepository commonRepository  = RevealApplication.getInstance().getContext().commonrepository(metadata().familyMemberRegister.tableName);
            CommonPersonObject child = commonRepository.findByBaseEntityId(childID);
            Map<String,String> childDetails = child.getColumnmaps();
            String dob = childDetails.get("dob");

            RevealJsonFormActivity activity = (RevealJsonFormActivity) context;
            JSONObject form = activity.getmJSONObject();
            JSONObject firstNameField = JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(form),"childFirstName");
            JSONObject lastNameField = JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(form),"surnameOfChild");
            JSONObject genderField = JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(form),"sex");

            try {
                firstNameField.put("value",childDetails.get("first_name"));
                firstNameField.put("read_only",true);
                lastNameField.put("value",childDetails.get("last_name"));
                lastNameField.put("read_only",true);
                String gender = childDetails.get("gender");
                genderField.put("value",gender.equalsIgnoreCase("male") ? "Male": "Female");
                genderField.put("read_only",true);
            } catch (JSONException e) {
                Timber.d(e);
            }

        }
    }

}
