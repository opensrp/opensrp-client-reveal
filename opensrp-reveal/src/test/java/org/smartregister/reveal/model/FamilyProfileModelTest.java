package org.smartregister.reveal.model;

import com.vijay.jsonwizard.utils.FormUtils;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.domain.Location;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.TestingUtils;
import org.smartregister.reveal.util.Utils;
import org.smartregister.util.Cache;
import org.smartregister.util.DateUtil;
import org.smartregister.util.JsonFormUtils;

import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LAST_NAME;
import static org.smartregister.reveal.util.FamilyConstants.DatabaseKeys.FAMILY_NAME;

/**
 * Created by samuelgithengi on 1/29/20.
 */
public class FamilyProfileModelTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private Location location = TestingUtils.getOperationalArea();

    private FamilyProfileModel familyProfileModel;

    @Mock
    public FormUtils formUtils;

    @Captor
    private ArgumentCaptor<String> formNameCaptor;

    private String familyJsonForm = "{\"count\":\"2\",\"encounter_type\":\"Update Family Registration\",\"entity_id\":\"2e1b9bc9-c437-42a6-ac12-3566ca620d3f\",\"relational_id\":\"\",\"metadata\":{\"start\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"start\",\"openmrs_entity_id\":\"163137AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"2020-01-29 02:47:40\"},\"end\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"end\",\"openmrs_entity_id\":\"163138AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"2020-01-29 02:47:44\"},\"today\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"encounter\",\"openmrs_entity_id\":\"encounter_date\",\"value\":\"29-01-2020\"},\"deviceid\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"deviceid\",\"openmrs_entity_id\":\"163149AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"000000000000000\"},\"subscriberid\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"subscriberid\",\"openmrs_entity_id\":\"163150AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"310270000000000\"},\"simserial\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"simserial\",\"openmrs_entity_id\":\"163151AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"89014103211118510720\"},\"phonenumber\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"phonenumber\",\"openmrs_entity_id\":\"163152AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"15555218135\"},\"encounter_location\":\"f2b5eec1-5cba-4dbe-a019-d6436ee2fa12\",\"look_up\":{\"entity_id\":\"\",\"value\":\"\"}},\"step1\":{\"title\":\"Family details\",\"fields\":[{\"key\":\"fam_name\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"first_name\",\"type\":\"edit_text\",\"hint\":\"First name of Head of Household\",\"edit_type\":\"name\",\"v_required\":{\"value\":\"true\",\"err\":\"Please enter first name of Head of Household\"},\"value\":\"Juka\"},{\"key\":\"old_fam_name\",\"type\":\"hidden\",\"value\":\"Jukap\"},{\"key\":\"house_number\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person_address\",\"openmrs_entity_id\":\"address2\",\"type\":\"edit_text\",\"hint\":\"House Number\",\"value\":\"\"},{\"key\":\"street\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person_address\",\"openmrs_entity_id\":\"street\",\"type\":\"edit_text\",\"hint\":\"Street\",\"value\":\"\"},{\"key\":\"landmark\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person_address\",\"openmrs_entity_id\":\"landmark\",\"type\":\"edit_text\",\"hint\":\"Landmark\",\"value\":\"\"}]},\"current_opensrp_id\":\"19225622_family\",\"invisible_required_fields\":\"[]\"}";

    private String memberJsonForm = "{\"count\":\"1\",\"encounter_type\":\"Update Family Member Registration\",\"entity_id\":\"0dd853ae-be8d-4bfc-956d-2fb91eb687e5\",\"relational_id\":\"\",\"metadata\":{\"start\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"start\",\"openmrs_entity_id\":\"163137AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"2020-01-29 03:31:19\"},\"end\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"end\",\"openmrs_entity_id\":\"163138AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"2020-01-29 03:31:41\"},\"today\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"encounter\",\"openmrs_entity_id\":\"encounter_date\",\"value\":\"29-01-2020\"},\"deviceid\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"deviceid\",\"openmrs_entity_id\":\"163149AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"000000000000000\"},\"subscriberid\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"subscriberid\",\"openmrs_entity_id\":\"163150AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"310270000000000\"},\"simserial\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"simserial\",\"openmrs_entity_id\":\"163151AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"89014103211118510720\"},\"phonenumber\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"phonenumber\",\"openmrs_entity_id\":\"163152AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"15555218135\"},\"encounter_location\":\"f2b5eec1-5cba-4dbe-a019-d6436ee2fa12\",\"look_up\":{\"entity_id\":\"\",\"value\":\"\"}},\"step1\":{\"title\":\"Edit Family Member\",\"fields\":[{\"key\":\"unique_id\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person_identifier\",\"openmrs_entity_id\":\"opensrp_id\",\"type\":\"edit_text\",\"hint\":\"ID\",\"read_only\":\"True\",\"v_required\":{\"value\":\"true\",\"err\":\"Please enter the ID\"},\"value\":\"19225622\"},{\"key\":\"first_name\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"edit_text\",\"hint\":\"First name\",\"edit_type\":\"name\",\"v_required\":{\"value\":\"true\",\"err\":\"Please enter the first name\"},\"relevance\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-member-relevance.yml\"}}},\"value\":\"Liu\",\"step\":\"step1\",\"is-rule-check\":true,\"is_visible\":true},{\"key\":\"first_name_as_fam_name\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"check_box\",\"label\":\" \",\"options\":[{\"key\":\"same_as_fam_name\",\"text\":\"First name same as household name\",\"text_size\":\"18px\",\"value\":false}],\"relevance\":{\"step1:is_family_head\":{\"type\":\"string\",\"ex\":\"equalTo(., \\\"true\\\")\"}},\"step\":\"step1\",\"is-rule-check\":true,\"is_visible\":true},{\"key\":\"surname\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"edit_text\",\"hint\":\"Surname\",\"edit_type\":\"name\",\"v_required\":{\"value\":\"true\",\"err\":\"Please enter the Surname\"},\"relevance\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-member-relevance.yml\"}}},\"value\":\"Juka\",\"step\":\"step1\",\"is-rule-check\":true,\"is_visible\":true},{\"key\":\"same_as_fam_name\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"check_box\",\"label\":\" \",\"options\":[{\"key\":\"same_as_fam_name\",\"text\":\"Surname same as head of family\",\"text_size\":\"18px\",\"value\":\"false\"}],\"relevance\":{\"step1:is_family_head\":{\"type\":\"string\",\"ex\":\"equalTo(., \\\"false\\\")\"}},\"step\":\"step1\",\"is-rule-check\":true,\"is_visible\":false},{\"key\":\"fam_name\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"hidden\",\"value\":\"Juka\",\"step\":\"step1\",\"is-rule-check\":true},{\"key\":\"is_family_head\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"hidden\",\"value\":\"true\",\"is-rule-check\":false},{\"key\":\"first_name_calculation\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"first_name\",\"type\":\"hidden\",\"calculation\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-member-calculation.yml\"}}},\"value\":\"Liu\"},{\"key\":\"surname_calculation\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"last_name\",\"type\":\"hidden\",\"calculation\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-member-calculation.yml\"}}},\"value\":\"Juka\"},{\"key\":\"dob\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"birthdate\",\"type\":\"date_picker\",\"hint\":\"Date of birth (DOB)\",\"expanded\":false,\"duration\":{\"label\":\"Age\"},\"min_date\":\"today-120y\",\"max_date\":\"today\",\"v_required\":{\"value\":\"true\",\"err\":\"Please enter the date of birth\"},\"relevance\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-member-relevance.yml\"}}},\"is_visible\":false},{\"key\":\"dob_unknown\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"birthdateApprox\",\"type\":\"check_box\",\"label\":\"\",\"options\":[{\"key\":\"dob_unknown\",\"text\":\"DOB unknown?\",\"text_size\":\"18px\",\"value\":true}],\"read_only\":false,\"value\":[\"dob_unknown\"],\"step\":\"step1\",\"is-rule-check\":true},{\"key\":\"age\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person_attribute\",\"openmrs_entity_id\":\"age\",\"type\":\"edit_text\",\"hint\":\"Age\",\"v_numeric_integer\":{\"value\":\"true\",\"err\":\"Please enter a number\"},\"v_min\":{\"value\":\"0\",\"err\":\"Age must be equal or greater than 0\"},\"v_max\":{\"value\":\"120\",\"err\":\"Age must be equal or less than 120\"},\"relevance\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-member-relevance.yml\"}}},\"v_required\":{\"value\":true,\"err\":\"Please enter the age\"},\"value\":\"55\",\"is_visible\":true,\"step\":\"step1\",\"is-rule-check\":true},{\"key\":\"sex\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"gender\",\"type\":\"native_radio\",\"label\":\"Sex\",\"options\":[{\"key\":\"Male\",\"text\":\"Male\"},{\"key\":\"Female\",\"text\":\"Female\"}],\"v_required\":{\"value\":\"true\",\"err\":\"Please enter the sex\"},\"value\":\"Male\"},{\"key\":\"phone_number\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_entity_id\":\"159635AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"type\":\"edit_text\",\"hint\":\"Phone number\",\"v_numeric\":{\"value\":\"true\",\"err\":\"Please enter a number\"},\"v_regex\":{\"value\":\"^$|0[0-9]{8,9}\",\"err\":\"Number must be 9-10 digits and must start with 0.\"},\"value\":\"\"},{\"key\":\"sleeps_outdoors\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"native_radio\",\"label\":\"Does this person spend the night outdoors?\",\"options\":[{\"key\":\"Yes\",\"text\":\"Yes\"},{\"key\":\"No\",\"text\":\"No\"}],\"v_required\":{\"value\":\"true\",\"err\":\"Please select whether this person spends the night outdoors\"},\"value\":\"Yes\"},{\"key\":\"occupation\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"edit_text\",\"hint\":\"Person's Occupation\",\"relevance\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-member-relevance.yml\"}}},\"value\":\"\",\"is_visible\":true},{\"key\":\"citizenship\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"native_radio\",\"label\":\"Citizenship\",\"options\":[{\"key\":\"Thai\",\"text\":\"Thai\"},{\"key\":\"Migrant-1\",\"text\":\"Migrant 1\"},{\"key\":\"Migrant-2\",\"text\":\"Migrant 2\"}],\"v_required\":{\"value\":true,\"err\":\"Please specify the Citizenship\"},\"value\":\"Thai\",\"is-rule-check\":false},{\"key\":\"national_id\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_entity_id\":\"163084AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"label_info_text\":\"What is their national identity number or their voter registration number?\",\"label_info_title\":\"National ID number\",\"type\":\"edit_text\",\"hint\":\"National ID number\",\"v_numeric\":{\"value\":\"true\",\"err\":\"Please enter a number\"},\"v_regex\":{\"value\":\"^$|[0-9]{13}\",\"err\":\"Number must be 13 digits.\"},\"relevance\":{\"step1:citizenship\":{\"type\":\"string\",\"ex\":\"equalTo(., \\\"Thai\\\")\"}},\"is_visible\":true}]},\"current_opensrp_id\":\"19225622\",\"invisible_required_fields\":\"[dob]\"}";

    @Before
    public void setUp() {
        String familyName = "Doe";
        familyProfileModel = new FamilyProfileModel(familyName);
        PreferencesUtil.getInstance().setCurrentOperationalArea(location.getId());
        Cache<Location> cache = new Cache<>();
        cache.get(location.getId(), () -> location);
        Whitebox.setInternalState(Utils.class, "cache", cache);
    }

    @Test
    public void testProcessFamilyRegistration() {
        FamilyEventClient eventClient = familyProfileModel.processFamilyRegistrationForm(familyJsonForm, "family1");

        assertNotNull(eventClient.getClient());
        assertEquals("Juka", eventClient.getClient().getFirstName());
        assertEquals("family1", eventClient.getClient().getRelationships().get("family").get(0));
        assertEquals("2e1b9bc9-c437-42a6-ac12-3566ca620d3f", eventClient.getClient().getBaseEntityId());
        assertNotNull(eventClient.getEvent());
        assertEquals("2e1b9bc9-c437-42a6-ac12-3566ca620d3f", eventClient.getEvent().getBaseEntityId());
        assertEquals(location.getId(), eventClient.getEvent().getLocationId());
        assertEquals(location.getId(), eventClient.getClient().getLocationId());
        assertEquals(8, eventClient.getEvent().getObs().size());
        assertEquals(FamilyConstants.EventType.UPDATE_FAMILY_REGISTRATION, eventClient.getEvent().getEventType());
    }


    @Test
    public void testProcessMemberRegistration() {
        FamilyEventClient eventClient = familyProfileModel.processMemberRegistration(memberJsonForm, "family1");
        assertNotNull(eventClient.getClient());
        assertEquals("Liu", eventClient.getClient().getFirstName());
        assertEquals("Juka", eventClient.getClient().getLastName());
        assertTrue(eventClient.getClient().getBirthdateApprox());
        assertEquals(org.smartregister.util.Utils.getDob(55), new SimpleDateFormat(DateUtil.DATE_FORMAT_FOR_TIMELINE_EVENT).format(eventClient.getClient().getBirthdate()));
        assertEquals("19225622", eventClient.getClient().getIdentifier("opensrp_id"));
        assertEquals("55", eventClient.getClient().getAttribute("age"));
        assertEquals("family1", eventClient.getClient().getRelationships().get("family").get(0));
        assertEquals("0dd853ae-be8d-4bfc-956d-2fb91eb687e5", eventClient.getClient().getBaseEntityId());
        assertNotNull(eventClient.getEvent());
        assertEquals("0dd853ae-be8d-4bfc-956d-2fb91eb687e5", eventClient.getEvent().getBaseEntityId());
        assertEquals(location.getId(), eventClient.getEvent().getLocationId());
        assertEquals(location.getId(), eventClient.getClient().getLocationId());
        assertEquals(14, eventClient.getEvent().getObs().size());
        assertEquals(FamilyConstants.EventType.FAMILY_MEMBER_REGISTRATION, eventClient.getEvent().getEventType());
    }


    @Test
    public void testProcessUpdateMemberRegistration() {
        FamilyEventClient eventClient = familyProfileModel.processUpdateMemberRegistration(memberJsonForm, "family1");
        assertNotNull(eventClient.getClient());
        assertEquals("Liu", eventClient.getClient().getFirstName());
        assertEquals("Juka", eventClient.getClient().getLastName());
        assertTrue(eventClient.getClient().getBirthdateApprox());
        assertEquals(org.smartregister.util.Utils.getDob(55), new SimpleDateFormat(DateUtil.DATE_FORMAT_FOR_TIMELINE_EVENT).format(eventClient.getClient().getBirthdate()));
        assertEquals("19225622", eventClient.getClient().getIdentifier("opensrp_id"));
        assertEquals("55", eventClient.getClient().getAttribute("age"));
        assertEquals("family1", eventClient.getClient().getRelationships().get("family").get(0));
        assertEquals("0dd853ae-be8d-4bfc-956d-2fb91eb687e5", eventClient.getClient().getBaseEntityId());
        assertNotNull(eventClient.getEvent());
        assertEquals("0dd853ae-be8d-4bfc-956d-2fb91eb687e5", eventClient.getEvent().getBaseEntityId());
        assertEquals(location.getId(), eventClient.getEvent().getLocationId());
        assertEquals(location.getId(), eventClient.getClient().getLocationId());
        assertEquals(14, eventClient.getEvent().getObs().size());
        assertEquals(FamilyConstants.EventType.UPDATE_FAMILY_MEMBER_REGISTRATION, eventClient.getEvent().getEventType());
    }

    @Test
    public void testGetFormAsJson() throws Exception {
        JSONObject formObject = new JSONObject(familyJsonForm);
        when(formUtils.getFormJsonFromRepositoryOrAssets(any(), any())).thenReturn(formObject);
        Whitebox.setInternalState(familyProfileModel, "formUtils", formUtils);
        JSONObject resultJson = familyProfileModel.getFormAsJson("json.form/family_register.json", "131", "131");

        verify(formUtils).getFormJsonFromRepositoryOrAssets(any(), formNameCaptor.capture());
        assertEquals("family_register", formNameCaptor.getValue());
        assertNotNull(resultJson);
    }

    @Test
    public void testGetFormAsJsonWithFamilyMember() throws Exception {

        HashMap<String,String> columns = new HashMap<>();
        columns.put(LAST_NAME, "random name");
        CommonPersonObject familyHead = new CommonPersonObject(UUID.randomUUID().toString(), null, null, null);
        familyHead.setColumnmaps(columns);
        familyProfileModel.setFamilyHeadPersonObject(familyHead);

        JSONObject formObject = new JSONObject(memberJsonForm);
        when(formUtils.getFormJsonFromRepositoryOrAssets(any(), any())).thenReturn(formObject);
        Whitebox.setInternalState(familyProfileModel, "formUtils", formUtils);
        JSONObject resultJson = familyProfileModel.getFormAsJson("family_member_register", "131", "131");

        verify(formUtils).getFormJsonFromRepositoryOrAssets(any(), formNameCaptor.capture());
        assertEquals("family_member_register", formNameCaptor.getValue());
        assertNotNull(resultJson);
        assertEquals("random name", JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(resultJson), FAMILY_NAME).getString("value"));

    }

    @Test
    public void testGetFormAsJsonWithNullForm() throws Exception {
        when(formUtils.getFormJsonFromRepositoryOrAssets(any(), any())).thenReturn(null);
        Whitebox.setInternalState(familyProfileModel, "formUtils", formUtils);
        JSONObject resultJson = familyProfileModel.getFormAsJson("null", "131", "131");
        assertNull(resultJson);
    }
}
