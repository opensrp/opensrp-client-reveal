package org.smartregister.reveal.util;

import android.app.Activity;
import android.content.Intent;

import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.activity.RevealJsonFormActivity;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Constants.JsonForm;

import static org.smartregister.reveal.util.Constants.JSON_FORM_PARAM_JSON;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;
import static org.smartregister.reveal.util.Constants.MOSQUITO_COLLECTION_EVENT;
import static org.smartregister.reveal.util.Constants.BEDNET_DISTRIBUTION_EVENT;
import static org.smartregister.reveal.util.Constants.BLOOD_SCREENING_EVENT;
import static org.smartregister.reveal.util.Constants.LARVAL_DIPPING_EVENT;
import static org.smartregister.reveal.util.Constants.REGISTER_STRUCTURE_EVENT;
import static org.smartregister.reveal.util.Constants.Intervention.IRS_VERIFICATION;
import static org.smartregister.reveal.util.Constants.BEHAVIOUR_CHANGE_COMMUNICATION;
import static org.smartregister.reveal.util.Constants.EventType.CASE_CONFIRMATION_EVENT;
import org.smartregister.reveal.model.MosquitoHarvestCardDetails;

import timber.log.Timber;

import static com.vijay.jsonwizard.constants.JsonFormConstants.VALUE;

/**
 * Created by Ephraim Kigamba - nek.eam@gmail.com on 04-08-2020.
 */
public class RevealJsonFormUtils extends org.smartregister.tasking.util.RevealJsonFormUtils {


    public void populatePAOTForm(MosquitoHarvestCardDetails cardDetails, JSONObject formJson) {
        if (formJson == null)
            return;
        try {
            populateField(formJson, Constants.JsonForm.PAOT_STATUS, cardDetails.getStatus(), Constants.CONFIGURATION.VALUE);
            populateField(formJson, Constants.JsonForm.PAOT_COMMENTS, cardDetails.getComments(), VALUE);
            populateField(formJson, Constants.JsonForm.LAST_UPDATED_DATE, cardDetails.getStartDate(), VALUE);
        } catch (JSONException e) {
            Timber.e(e);
        }
    }


    @Override
    public String getFormName(String encounterType, String taskCode) {
        String formName = null;
        if (SPRAY_EVENT.equals(encounterType) || Intervention.IRS.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.NAMIBIA) {
                formName = JsonForm.SPRAY_FORM_NAMIBIA;
            } else if (BuildConfig.BUILD_COUNTRY == Country.BOTSWANA) {
                formName = JsonForm.SPRAY_FORM_BOTSWANA;
            } else if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
                formName = JsonForm.SPRAY_FORM_ZAMBIA;
            } else if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = JsonForm.THAILAND_SPRAY_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = JsonForm.SPRAY_FORM_REFAPP;
            } else {
                formName = JsonForm.SPRAY_FORM;
            }
        } else if (MOSQUITO_COLLECTION_EVENT.equals(encounterType)
                || Intervention.MOSQUITO_COLLECTION.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = JsonForm.THAILAND_MOSQUITO_COLLECTION_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = JsonForm.REFAPP_MOSQUITO_COLLECTION_FORM;
            } else {
                formName = JsonForm.MOSQUITO_COLLECTION_FORM;
            }
        } else if (BEDNET_DISTRIBUTION_EVENT.equals(encounterType)
                || Intervention.BEDNET_DISTRIBUTION.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = JsonForm.THAILAND_BEDNET_DISTRIBUTION_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = JsonForm.REFAPP_BEDNET_DISTRIBUTION_FORM;
            } else {
                formName = JsonForm.BEDNET_DISTRIBUTION_FORM;
            }
        } else if (CASE_CONFIRMATION_EVENT.equals(encounterType)
                || Intervention.CASE_CONFIRMATION.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = JsonForm.THAILAND_CASE_CONFIRMATION_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = JsonForm.REFAPP_CASE_CONFIRMATION_FORM;
            } else {
                formName = JsonForm.CASE_CONFIRMATION_FORM;
            }
        } else if (BLOOD_SCREENING_EVENT.equals(encounterType)
                || Intervention.BLOOD_SCREENING.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = JsonForm.THAILAND_BLOOD_SCREENING_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = JsonForm.REFAPP_BLOOD_SCREENING_FORM;
            } else {
                formName = JsonForm.BLOOD_SCREENING_FORM;
            }
        } else if (LARVAL_DIPPING_EVENT.equals(encounterType) || Intervention.LARVAL_DIPPING.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = JsonForm.THAILAND_LARVAL_DIPPING_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = JsonForm.REFAPP_LARVAL_DIPPING_FORM;
            } else {
                formName = JsonForm.LARVAL_DIPPING_FORM;
            }
        } else if (BEHAVIOUR_CHANGE_COMMUNICATION.equals(encounterType) || Intervention.BCC.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = JsonForm.THAILAND_BEHAVIOUR_CHANGE_COMMUNICATION_FORM;
            } else {
                formName = JsonForm.BEHAVIOUR_CHANGE_COMMUNICATION_FORM;
            }
        } else if (REGISTER_STRUCTURE_EVENT.equals(encounterType)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = JsonForm.THAILAND_ADD_STRUCTURE_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.NAMIBIA) {
                formName = JsonForm.NAMIBIA_ADD_STRUCTURE_FORM;
            } else {
                formName = JsonForm.ADD_STRUCTURE_FORM;
            }
        } else if (Constants.EventType.PAOT_EVENT.equals(encounterType) || Intervention.PAOT.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
                formName = JsonForm.THAILAND_PAOT_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = JsonForm.REFAPP_PAOT_FORM;
            } else {
                formName = JsonForm.PAOT_FORM;
            }
        } else if (Intervention.MDA_ADHERENCE.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
                formName = JsonForm.ZAMBIA_MDA_ADHERENCE_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = JsonForm.REFAPP_MDA_ADHERENCE_FORM;
            }
        } else if (Intervention.MDA_DISPENSE.equals(taskCode)) {
            if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
                formName = JsonForm.ZAMBIA_MDA_DISPENSE_FORM;
            } else if (BuildConfig.BUILD_COUNTRY == Country.REFAPP) {
                formName = JsonForm.REFAPP_MDA_DISPENSE_FORM;
            }
        } else if (IRS_VERIFICATION.equals(encounterType) || Intervention.IRS_VERIFICATION.equals(taskCode)) {
            formName = JsonForm.ZAMBIA_IRS_VERIFICATION_FORM;
        }
        return formName;
    }

    @Override
    public String getFormName(String encounterType) {
        return getFormName(encounterType, null);
    }

    @Override
    public void startJsonForm(JSONObject form, Activity context, int requestCode) {
        Intent intent = new Intent(context, RevealJsonFormActivity.class);
        try {
            intent.putExtra(JSON_FORM_PARAM_JSON, form.toString());
            context.startActivityForResult(intent, requestCode);
        } catch (Exception e) {
            Timber.e(e);
        }
    }

}
