package org.smartregister.reveal.util;

import android.content.Context;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.model.MosquitoHarvestCardDetails;
import org.smartregister.reveal.util.Constants.JsonForm;
import org.smartregister.util.AssetHandler;
import org.smartregister.util.JsonFormUtils;

import static com.vijay.jsonwizard.constants.JsonFormConstants.TEXT;
import static org.junit.Assert.assertEquals;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.Intervention.PAOT;
import static org.smartregister.reveal.util.Constants.LARVAL_DIPPING_EVENT;
import static org.smartregister.reveal.util.Constants.MOSQUITO_COLLECTION_EVENT;

/**
 * Created by Vincent Karuri on 25/04/2019
 */
public class RevealJsonFormUtilsTest extends BaseUnitTest {

    private RevealJsonFormUtils revealJsonFormUtils;

    private Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        revealJsonFormUtils = new RevealJsonFormUtils();
    }

    @Test
    public void testGetFormNameShouldReturnThailandLarvalDippingFormForLarvalDippingEvent() {
        Country buildCountry = BuildConfig.BUILD_COUNTRY;
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.ZAMBIA);
        assertEquals(JsonForm.LARVAL_DIPPING_FORM, revealJsonFormUtils.getFormName(LARVAL_DIPPING_EVENT, null));
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, buildCountry);
    }

    @Test
    public void testGetFormNameShouldReturnThailandLarvalDippingFormForLarvalDippingIntervention() {
        assertEquals(revealJsonFormUtils.getFormName(null, LARVAL_DIPPING), JsonForm.LARVAL_DIPPING_FORM);
    }

    @Test
    public void testGetFormNameShouldReturnThailandMosquitoCollectionFormForLarvalDippingEvent() {
        Country buildCountry = BuildConfig.BUILD_COUNTRY;
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.ZAMBIA);
        assertEquals(JsonForm.MOSQUITO_COLLECTION_FORM, revealJsonFormUtils.getFormName(MOSQUITO_COLLECTION_EVENT, null));
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, buildCountry);
    }

    @Test
    public void testGetFormNameShouldReturnThailandMosquitoCollectionFormForLarvalDippingIntervention() {
        assertEquals(revealJsonFormUtils.getFormName(null, MOSQUITO_COLLECTION), JsonForm.MOSQUITO_COLLECTION_FORM);
    }

    @Test
    public void testGetFormNameShouldReturnPAOTForm() {
        assertEquals(JsonForm.PAOT_FORM, revealJsonFormUtils.getFormName(null, Constants.EventType.PAOT_EVENT));
    }

    @Test
    public void testGetFormNameShouldReturnThailandPAOTForm() {
        Country buildCountry = BuildConfig.BUILD_COUNTRY;
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.THAILAND);
        assertEquals(JsonForm.THAILAND_PAOT_FORM, revealJsonFormUtils.getFormName(null, Constants.EventType.PAOT_EVENT));
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, buildCountry);
    }


    @Test
    public void testPopulatePAOTForm() throws JSONException {
        MosquitoHarvestCardDetails cardDetails = new MosquitoHarvestCardDetails("Active", "2019-07-30", null, PAOT);
        cardDetails.setComments("Paot point");
        JSONObject form = new JSONObject(AssetHandler.readFileFromAssetsFolder(JsonForm.PAOT_FORM, context));
        revealJsonFormUtils.populatePAOTForm(cardDetails, form);
        assertEquals("Active", JsonFormUtils.getFieldValue(JsonFormUtils.fields(form), JsonForm.PAOT_STATUS));
        assertEquals("2019-07-30", JsonFormUtils.getFieldValue(JsonFormUtils.fields(form), JsonForm.LAST_UPDATED_DATE));
        assertEquals("Paot point", JsonFormUtils.getFieldValue(JsonFormUtils.fields(form), JsonForm.PAOT_COMMENTS));
    }

    @Test
    public void testPopulateField() throws JSONException {
        JSONObject form = new JSONObject(AssetHandler.readFileFromAssetsFolder(JsonForm.ADD_STRUCTURE_FORM, context));
        assertEquals("", JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(form), JsonForm.SELECTED_OPERATIONAL_AREA_NAME).get(TEXT).toString());

        revealJsonFormUtils.populateField(form, Constants.JsonForm.SELECTED_OPERATIONAL_AREA_NAME, "TLV1", TEXT);
        assertEquals("TLV1", JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(form), JsonForm.SELECTED_OPERATIONAL_AREA_NAME).get(TEXT));
    }
}
