package org.smartregister.reveal.util;

import org.json.JSONException;
import org.json.JSONObject;
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
}
