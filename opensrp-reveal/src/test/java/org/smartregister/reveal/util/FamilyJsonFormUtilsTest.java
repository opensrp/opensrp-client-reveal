package org.smartregister.reveal.util;

import android.content.Context;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.util.Constants;
import org.smartregister.family.util.DBConstants.KEY;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.FamilyConstants.DatabaseKeys;
import org.smartregister.reveal.util.FamilyConstants.FormKeys;
import org.smartregister.util.AssetHandler;
import org.smartregister.util.DateUtil;
import org.smartregister.util.FormUtils;
import org.smartregister.util.JsonFormUtils;
import org.smartregister.view.LocationPickerView;

import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;
import static org.smartregister.family.util.Constants.JSON_FORM_KEY.ENCOUNTER_LOCATION;
import static org.smartregister.family.util.Constants.JSON_FORM_KEY.OPTIONS;
import static org.smartregister.family.util.DBConstants.KEY.DOB;
import static org.smartregister.family.util.DBConstants.KEY.UNIQUE_ID;
import static org.smartregister.family.util.JsonFormUtils.CURRENT_OPENSRP_ID;
import static org.smartregister.family.util.JsonFormUtils.METADATA;
import static org.smartregister.reveal.util.FamilyConstants.EventType.UPDATE_FAMILY_MEMBER_REGISTRATION;
import static org.smartregister.reveal.util.FamilyConstants.EventType.UPDATE_FAMILY_REGISTRATION;
import static org.smartregister.reveal.util.FamilyConstants.JSON_FORM.FAMILY_MEMBER_REGISTER;
import static org.smartregister.reveal.util.FamilyConstants.JSON_FORM.FAMILY_UPDATE;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY_MEMBER;
import static org.smartregister.util.JsonFormUtils.VALUE;

/**
 * Created by samuelgithengi on 5/30/19.
 */
public class FamilyJsonFormUtilsTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private LocationPickerView lpv;

    @Mock
    private FormUtils formUtils;

    @Mock
    private LocationHelper locationHelper;

    private Context context = RuntimeEnvironment.application;

    private CommonPersonObjectClient client = TestingUtils.getCommonPersonObjectClient();

    private FamilyJsonFormUtils familyJsonFormUtils;

    @Test
    public void testGetAutoPopulatedJsonEditFormStringWithoutForm() {
        familyJsonFormUtils = new FamilyJsonFormUtils(lpv, formUtils, locationHelper, context);
        client.getColumnmaps().put(KEY.VILLAGE_TOWN, "The Luang");
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditFormString(FAMILY_UPDATE, context, client, UPDATE_FAMILY_REGISTRATION);
        assertNull(form);
    }


    @Test
    public void testGetAutoPopulatedJsonEditFormString() throws JSONException {
        String formString = AssetHandler.readFileFromAssetsFolder("json.form/" + FAMILY_UPDATE + ".json", context);
        when(formUtils.getFormJson(FAMILY_UPDATE)).thenReturn(new JSONObject(formString));
        familyJsonFormUtils = new FamilyJsonFormUtils(lpv, formUtils, locationHelper, context);
        client.getColumnmaps().put(KEY.LANDMARK, "The Luang");
        assertNull("The Luang", JsonFormUtils.getFieldValue(formString, KEY.VILLAGE_TOWN));
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditFormString(FAMILY_UPDATE, context, client, UPDATE_FAMILY_REGISTRATION);
        assertEquals("The Luang", JsonFormUtils.getFieldValue(form.toString(), KEY.LANDMARK));
    }

    @Test
    public void testProcessPopulatableVillage() throws JSONException {
        client.getColumnmaps().put(KEY.VILLAGE_TOWN, "The Luang");
        JSONObject form = new JSONObject();
        form.put(JsonFormUtils.KEY, KEY.VILLAGE_TOWN);
        FamilyJsonFormUtils.processPopulatableFields(client, form);
        assertEquals("The Luang", form.getString(VALUE));

    }


    @Test
    public void testProcessPopulatableHouse() throws JSONException {

        client.getColumnmaps().put(DatabaseKeys.HOUSE_NUMBER, "124");
        JSONObject form = new JSONObject();
        form.put(JsonFormUtils.KEY, DatabaseKeys.HOUSE_NUMBER);
        FamilyJsonFormUtils.processPopulatableFields(client, form);
        assertEquals("124", form.getString(VALUE));


    }


    @Test
    public void testProcessPopulatableField() throws JSONException {

        client.getColumnmaps().put(KEY.STREET, "Brao Street");
        JSONObject form = new JSONObject();
        form.put(JsonFormUtils.KEY, KEY.STREET);
        FamilyJsonFormUtils.processPopulatableFields(client, form);
        assertEquals("Brao Street", form.getString(VALUE));

    }


    @Test
    public void testProcessPopulatableLandmark() throws JSONException {

        client.getColumnmaps().put(KEY.LANDMARK, "Lao Falls");
        JSONObject form = new JSONObject();
        form.put(JsonFormUtils.KEY, KEY.LANDMARK);
        FamilyJsonFormUtils.processPopulatableFields(client, form);
        assertEquals("Lao Falls", form.getString(VALUE));

    }

    @Test
    public void testProcessPopulatableFamilyName() throws JSONException {

        JSONObject form = new JSONObject();
        form.put(JsonFormUtils.KEY, DatabaseKeys.FAMILY_NAME);
        FamilyJsonFormUtils.processPopulatableFields(client, form);
        assertEquals("Charity", form.getString(VALUE));

    }


    @Test
    public void testProcessPopulatableOtherFields() throws JSONException {
        String uniqueId = "0ffa3ebd94124eacb8";
        client.getColumnmaps().put(UNIQUE_ID, uniqueId);
        JSONObject form = new JSONObject();
        form.put(JsonFormUtils.KEY, UNIQUE_ID);
        FamilyJsonFormUtils.processPopulatableFields(client, form);
        assertEquals(uniqueId, form.getString(VALUE));

    }


    private void setupMemberForm() throws JSONException {
        String formString = AssetHandler.readFileFromAssetsFolder("json.form/" + FAMILY_MEMBER_REGISTER + ".json", context);
        when(formUtils.getFormJson(FAMILY_MEMBER_REGISTER)).thenReturn(new JSONObject(formString));
        familyJsonFormUtils = new FamilyJsonFormUtils(lpv, formUtils, locationHelper, context);

    }

    @Test
    public void testGetAutoPopulatedJsonEditMemberFormString() throws JSONException {
        setupMemberForm();
        String locationId = UUID.randomUUID().toString();
        when(locationHelper.getOpenMrsLocationId(null)).thenReturn(locationId);
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditMemberFormString(R.string.edit_member_form_title, FAMILY_MEMBER_REGISTER, client, UPDATE_FAMILY_MEMBER_REGISTRATION, "Ker");
        assertEquals(locationId, form.getJSONObject(METADATA).getString(ENCOUNTER_LOCATION));
        assertEquals("12987632", form.getString(CURRENT_OPENSRP_ID));
        assertEquals("Charity", JsonFormUtils.getFieldValue(form.toString(), KEY.FIRST_NAME));
        assertEquals("Otala", JsonFormUtils.getFieldValue(form.toString(), FormKeys.SURNAME));
        assertEquals("Ker", JsonFormUtils.getFieldValue(form.toString(), DatabaseKeys.FAMILY_NAME));
        String dobString = "1982-01-01";
        String ageStr = org.smartregister.util.Utils.getDuration(dobString);
        String age = ageStr.substring(0, ageStr.indexOf("y"));
        assertEquals(age, JsonFormUtils.getFieldValue(form.toString(), DatabaseKeys.AGE));
        assertEquals(DateUtil.formatDate(dobString, DateUtil.DATE_FORMAT_FOR_TIMELINE_EVENT), JsonFormUtils.getFieldValue(form.toString(), DOB));
    }


    @Test
    public void testGetAutoPopulatedJsonEditMemberFormStringForEstimateDob() throws JSONException {
        setupMemberForm();
        client.getColumnmaps().put(Constants.JSON_FORM_KEY.DOB_UNKNOWN, "true");
        client.getColumnmaps().put(DatabaseKeys.AGE, "37");
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditMemberFormString(R.string.edit_member_form_title, FAMILY_MEMBER_REGISTER, client, UPDATE_FAMILY_MEMBER_REGISTRATION, "Ker");
        assertEquals("37", JsonFormUtils.getFieldValue(form.toString(), DatabaseKeys.AGE));
        String dobString = org.smartregister.util.Utils.getDob(37);
        assertEquals(dobString, JsonFormUtils.getFieldValue(form.toString(), DOB));
        assertTrue(JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(form), Constants.JSON_FORM_KEY.DOB_UNKNOWN).getJSONArray(OPTIONS).getJSONObject(0).getBoolean(VALUE));

    }


    @Test
    public void testFirstNameCalc() throws JSONException {
        setupMemberForm();
        String familyName = "Ker";
        client.getColumnmaps().put(KEY.FIRST_NAME, familyName);
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditMemberFormString(R.string.edit_member_form_title, FAMILY_MEMBER_REGISTER, client, UPDATE_FAMILY_MEMBER_REGISTRATION, familyName);
        assertEquals("", JsonFormUtils.getFieldValue(form.toString(), FormKeys.FIRST_NAME));
        assertTrue(JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(form), "same_as_fam_name").getJSONArray(OPTIONS).getJSONObject(0).getBoolean(VALUE));
    }


    @Test
    public void testSurnameMappedFields() throws JSONException {
        setupMemberForm();
        String citizenship = "Thai";
        client.getColumnmaps().put(DatabaseKeys.CITIZENSHIP, "Thai");
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditMemberFormString(R.string.edit_member_form_title, FAMILY_MEMBER_REGISTER, client, UPDATE_FAMILY_MEMBER_REGISTRATION, citizenship);
        assertEquals(citizenship, JsonFormUtils.getFieldValue(form.toString(), DatabaseKeys.CITIZENSHIP));
    }

    @Test
    public void testCreateUpdateMemberSurnameEvent() {
        String baseEntityId = UUID.randomUUID().toString();
        Event updateFamilyEvent = new Event().withEventType(UPDATE_FAMILY_MEMBER_REGISTRATION);
        AllSharedPreferences allSharedPreferences = RevealApplication.getInstance().getContext().allSharedPreferences();
        allSharedPreferences.updateANMUserName("user1132");
        allSharedPreferences.saveDefaultTeam("user1132", "Ateam");
        allSharedPreferences.saveDefaultTeamId("user1132", "2342fsdfds99");
        allSharedPreferences.saveDefaultLocalityId("user1132", "13k083-jhnf33");
        Event event = FamilyJsonFormUtils.createUpdateMemberNameEvent(baseEntityId, updateFamilyEvent);
        assertEquals("13k083-jhnf33", event.getLocationId());
        assertEquals("user1132", event.getProviderId());
        assertEquals("2342fsdfds99", event.getTeamId());
        assertEquals("Ateam", event.getTeam());
        assertEquals(UPDATE_FAMILY_MEMBER_REGISTRATION, event.getEventType());
        assertEquals(FAMILY_MEMBER, event.getEntityType());
    }

}
