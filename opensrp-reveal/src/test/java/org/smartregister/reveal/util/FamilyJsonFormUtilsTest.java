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
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.util.DBConstants.KEY;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.util.FamilyConstants.DatabaseKeys;
import org.smartregister.util.AssetHandler;
import org.smartregister.util.FormUtils;
import org.smartregister.util.JsonFormUtils;
import org.smartregister.view.LocationPickerView;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.when;
import static org.smartregister.family.util.DBConstants.KEY.UNIQUE_ID;
import static org.smartregister.reveal.util.FamilyConstants.EventType.UPDATE_FAMILY_REGISTRATION;
import static org.smartregister.reveal.util.FamilyConstants.JSON_FORM.FAMILY_UPDATE;
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

    @Test
    public void testGetAutoPopulatedJsonEditFormStringWithoutForm() {
        FamilyJsonFormUtils familyJsonFormUtils = new FamilyJsonFormUtils(lpv, formUtils, locationHelper, context);
        client.getColumnmaps().put(KEY.VILLAGE_TOWN, "The Luang");
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditFormString(FAMILY_UPDATE, context, client, UPDATE_FAMILY_REGISTRATION);
        assertNull(form);
    }


    @Test
    public void testGetAutoPopulatedJsonEditFormString() throws JSONException {
        String formString = AssetHandler.readFileFromAssetsFolder("json.form/" + FAMILY_UPDATE + ".json", context);
        when(formUtils.getFormJson(FAMILY_UPDATE)).thenReturn(new JSONObject(formString));
        FamilyJsonFormUtils familyJsonFormUtils = new FamilyJsonFormUtils(lpv, formUtils, locationHelper, context);
        client.getColumnmaps().put(KEY.VILLAGE_TOWN, "The Luang");
        assertNull("The Luang", JsonFormUtils.getFieldValue(formString, KEY.VILLAGE_TOWN));
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditFormString(FAMILY_UPDATE, context, client, UPDATE_FAMILY_REGISTRATION);
        assertEquals("The Luang", JsonFormUtils.getFieldValue(form.toString(), KEY.VILLAGE_TOWN));
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
}
