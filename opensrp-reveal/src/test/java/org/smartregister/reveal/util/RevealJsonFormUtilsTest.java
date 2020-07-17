package org.smartregister.reveal.util;

import android.content.Context;

import com.mapbox.geojson.Feature;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.domain.Geometry;
import org.smartregister.domain.Location;
import org.smartregister.domain.LocationProperty;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.model.MosquitoHarvestCardDetails;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.Constants.JsonForm;
import org.smartregister.util.AssetHandler;
import org.smartregister.util.JsonFormUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static com.vijay.jsonwizard.constants.JsonFormConstants.TEXT;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.smartregister.reveal.util.Constants.BLOOD_SCREENING_EVENT;
import static org.smartregister.reveal.util.Constants.EventType.CASE_CONFIRMATION_EVENT;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.BEDNET_DISTRIBUTION_EVENT;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.Intervention.PAOT;
import static org.smartregister.reveal.util.Constants.LARVAL_DIPPING_EVENT;
import static org.smartregister.reveal.util.Constants.MOSQUITO_COLLECTION_EVENT;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;
import static org.smartregister.reveal.util.Constants.STRUCTURE;
import static org.smartregister.reveal.util.Constants.TASK_RESET_EVENT;
import static org.smartregister.reveal.util.Utils.getPropertyValue;

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

    @Test
    public void testgetFormJSON() throws JSONException  {
        StructureTaskDetails task = new StructureTaskDetails("d12202fb-d347-4d7a-8859-fb370304c34c");
        task.setBusinessStatus("Not Visited");
        task.setTaskEntity("c72310fd-9c60-403e-a6f8-e38bf5d6359b");
        task.setStructureId("e5246812-f66c-41d9-8739-464f913b112d");
        task.setTaskCode("Blood Screening");
        task.setTaskStatus("READY");
        task.setTaskName("Yogi  Feri, 9");
        task.setTaskAction("Record\n" +"Screening");

        Location structure = new Location();
        structure.setId("e5246812-f66c-41d9-8739-464f913b112d");
        structure.setServerVersion(1569490867604L);
        structure.setSyncStatus("Synced");
        structure.setType("Feature");
        structure.setJurisdiction(false);

        Geometry g = new Geometry();
        g.setType(Geometry.GeometryType.MULTI_POLYGON);
        structure.setGeometry(g);

        LocationProperty lp = new LocationProperty();
        lp.setParentId("6fffaf7f-f16f-4713-a1ac-0cf6e2fe7f2a");
        HashMap<String, String> hm = new HashMap<>();
        hm.put("houseNumber", "6533");
        lp.setCustomProperties(hm);
        lp.setStatus(LocationProperty.PropertyStatus.ACTIVE);
        structure.setProperties(lp);

        JSONObject jsonObject = revealJsonFormUtils.getFormJSON(context, JsonForm.BLOOD_SCREENING_FORM, task, structure);
        assertEquals(jsonObject.getJSONObject("details").getString(Constants.Properties.FORM_VERSION), "0.0.1");
    }

    @Test
    public void testGetFormJsonFromFeature() throws JSONException {
        Feature structure = TestingUtils.getStructure();
        String expectedTaskIdentifier = getPropertyValue(structure, Constants.Properties.TASK_IDENTIFIER);
        JSONObject jsonObject = revealJsonFormUtils.getFormJSON(context, JsonForm.SPRAY_FORM, structure, Constants.BusinessStatus.SPRAYED, "John");
        assertNotNull(jsonObject);
        assertEquals(structure.id(), jsonObject.getJSONObject("details").getString(Constants.Properties.LOCATION_ID));
        assertEquals(expectedTaskIdentifier, jsonObject.getJSONObject("details").getString(Constants.Properties.TASK_IDENTIFIER));
    }

    @Test
    public void testCreateEvent() {

        String baseEntityId = UUID.randomUUID().toString();
        String locationId = UUID.randomUUID().toString();
        Map<String, String> details = new HashMap<>();
        String  eventType =  TASK_RESET_EVENT;
        String entityType = STRUCTURE;

        Event actualEvent = revealJsonFormUtils.createTaskEvent(baseEntityId, locationId, details, eventType, entityType);

        assertEquals(baseEntityId, actualEvent.getBaseEntityId());
        assertEquals(locationId, actualEvent.getLocationId());
        assertEquals(eventType, actualEvent.getEventType());
        assertEquals(entityType, actualEvent.getEntityType());
        assertEquals(baseEntityId, actualEvent.getBaseEntityId());
    }

    @Test
    public void testGetNamibiaSprayForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.NAMIBIA);
        String actualFormName = revealJsonFormUtils.getFormName(SPRAY_EVENT, IRS);
        assertEquals(JsonForm.SPRAY_FORM_NAMIBIA, actualFormName);
    }

    @Test
    public void testGetBotswanaSprayForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.BOTSWANA);
        String actualFormName = revealJsonFormUtils.getFormName(SPRAY_EVENT, IRS);
        assertEquals(JsonForm.SPRAY_FORM_BOTSWANA, actualFormName);
    }

    @Test
    public void testGetZambiaSprayForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.ZAMBIA);
        String actualFormName = revealJsonFormUtils.getFormName(SPRAY_EVENT, IRS);
        assertEquals(JsonForm.SPRAY_FORM_ZAMBIA, actualFormName);
    }

    @Test
    public void testGetThailandSprayForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.THAILAND);
        String actualFormName = revealJsonFormUtils.getFormName(SPRAY_EVENT, IRS);
        assertEquals(JsonForm.THAILAND_SPRAY_FORM, actualFormName);
    }

    @Test
    public void testGetRefAppSprayForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.REFAPP);
        String actualFormName = revealJsonFormUtils.getFormName(SPRAY_EVENT, IRS);
        assertEquals(JsonForm.SPRAY_FORM_REFAPP, actualFormName);
    }

    @Test
    public void testGetThailandBednetDistributionForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.THAILAND);
        String actualFormName = revealJsonFormUtils.getFormName(BEDNET_DISTRIBUTION_EVENT, BEDNET_DISTRIBUTION);
        assertEquals(JsonForm.THAILAND_BEDNET_DISTRIBUTION_FORM, actualFormName);
    }

    @Test
    public void testGetRefAppBednetDistributionForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.REFAPP);
        String actualFormName = revealJsonFormUtils.getFormName(BEDNET_DISTRIBUTION_EVENT, BEDNET_DISTRIBUTION);
        assertEquals(JsonForm.REFAPP_BEDNET_DISTRIBUTION_FORM, actualFormName);
    }

    @Test
    public void testGetThailandCaseConfirmationForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.THAILAND);
        String actualFormName = revealJsonFormUtils.getFormName(CASE_CONFIRMATION_EVENT, CASE_CONFIRMATION);
        assertEquals(JsonForm.THAILAND_CASE_CONFIRMATION_FORM, actualFormName);
    }

    @Test
    public void testGetRefAppCaseConfirmationForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.REFAPP);
        String actualFormName = revealJsonFormUtils.getFormName(CASE_CONFIRMATION_EVENT, CASE_CONFIRMATION);
        assertEquals(JsonForm.REFAPP_CASE_CONFIRMATION_FORM, actualFormName);
    }


    @Test
    public void testGetBednetDistributionForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.BOTSWANA);
        String actualFormName = revealJsonFormUtils.getFormName(BEDNET_DISTRIBUTION_EVENT, BEDNET_DISTRIBUTION);
        assertEquals(JsonForm.BEDNET_DISTRIBUTION_FORM, actualFormName);
    }

    @Test
    public void testGetCaseConfirmationDistributionForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.BOTSWANA);
        String actualFormName = revealJsonFormUtils.getFormName(CASE_CONFIRMATION_EVENT, CASE_CONFIRMATION);
        assertEquals(JsonForm.CASE_CONFIRMATION_FORM, actualFormName);
    }

    @Test
    public void testGetThailandBloodScreeningForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.THAILAND);
        String actualFormName = revealJsonFormUtils.getFormName(BLOOD_SCREENING_EVENT, BLOOD_SCREENING);
        assertEquals(JsonForm.THAILAND_BLOOD_SCREENING_FORM, actualFormName);
    }

}

