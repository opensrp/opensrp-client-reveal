package org.smartregister.reveal.util;

public interface Constants {

    String VIEW_CONFIGURATION_PREFIX = "ViewConfiguration_";

    String FILTER_TEAM_ID = "teamId";

    String JSON_FORM_PARAM_JSON = "json";

    int REQUEST_CODE_GET_JSON = 3432;

    int REQUEST_CODE_GET_JSON_FRAGMENT = 3439;

    String METADATA = "metadata";

    String DETAILS = "details";

    String ENTITY_ID = "entity_id";

    String SPRAY_EVENT = "Spray";

    String REGISTER_STRUCTURE_EVENT = "Register_Structure";

    String MOSQUITO_COLLECTION_EVENT = "mosquito_collection";

    String LARVAL_DIPPING_EVENT = "larval_dipping";

    String BEDNET_DISTRIBUTION_EVENT = "bednet_distribution";

    String BLOOD_SCREENING_EVENT = "blood_screening";

    String CASE_CONFIRMATION_EVENT = "case_confirmation";

    String STRUCTURE = "Structure";

    String START_DATE = "start_date";

    String END_DATE = "end_date";

    String STATUS = "status";

    double MY_LOCATION_ZOOM_LEVEL = 17.5; // modifying this will also necessitate modifying the VERTICAL_OFFSET

    double VERTICAL_OFFSET = -0.0003;

    double REFRESH_MAP_MINIMUM_DISTANCE = 5;

    int ANIMATE_TO_LOCATION_DURATION = 1000;

    interface CONFIGURATION {
        String LOGIN = "login";
        String GLOBAL_CONFIGS = "global_configs";
        String KEY = "key";
        String VALUE = "value";
        String SETTINGS = "settings";
        String LOCATION_BUFFER_RADIUS_IN_METRES = "location_buffer_radius_in_metres";
        Float DEFAULT_LOCATION_BUFFER_RADIUS_IN_METRES = 25f;
        String UPDATE_LOCATION_BUFFER_RADIUS = "update_location_buffer_radius";
    }

    interface Preferences {
        String CURRENT_FACILITY = "CURRENT_FACILITY";
        String CURRENT_DISTRICT = "CURRENT_DISTRICT";
        String CURRENT_CAMPAIGN = "CURRENT_CAMPAIGN";
        String CURRENT_CAMPAIGN_ID = "CURRENT_CAMPAIGN_ID";
        String FACILITY_LEVEL = "FACILITY_LEVEL";
    }

    interface Tags {
        String COUNTRY = "Country";
        String PROVINCE = "Province";
        String DISTRICT = "District";
        String HEALTH_CENTER = "Rural Health Centre";
        String CANTON = "Canton";
        String VILLAGE = "Village";
        String OPERATIONAL_AREA = "Operational Area";
    }

    interface Properties {
        String TASK_IDENTIFIER = "taskIdentifier";
        String TASK_BUSINESS_STATUS = "taskBusinessStatus";
        String TASK_STATUS = "taskStatus";
        String TASK_CODE = "taskCode";
        String LOCATION_UUID = "locationUUID";
        String LOCATION_VERSION = "locationVersion";
        String LOCATION_TYPE = "locationType";
        String LOCATION_PARENT = "locationParent";
        String LOCATION_ID = "location_id";
    }


    interface GeoJSON {
        String TYPE = "type";
        String FEATURE_COLLECTION = "FeatureCollection";
        String FEATURES = "features";
    }

    interface Intervention {
        String IRS = "IRS";

        String MDA = "MDA";

        String MOSQUITO_COLLECTION = "Mosquito Collection";

        String LARVAL_DIPPING = "Larval Dipping";

        String IRS_VISIT = "IRS Visit";

        String BCC = "BCC";

        String BEDNET_DISTRIBUTION = "Bednet Distribution";

        String BLOOD_SCREENING = "Blood Screening";

        String CASE_CONFIRMATION = "Case Confirmation";

        String REGISTER_FAMILY = "RACD Register Family";
    }

    interface Tables {
        String MOSQUITO_COLLECTIONS_TABLE = "mosquito_collections";
        String LARVAL_DIPPINGS_TABLE = "larval_dippings";
    }

    interface BusinessStatus {
        String NOT_VISITED = "Not Visited";
        String NOT_SPRAYED = "Not Sprayed";
        String SPRAYED = "Sprayed";
        String NOT_SPRAYABLE = "Not Sprayable";
        String COMPLETE = "Complete";
        String INCOMPLETE = "Incomplete";
        String NOT_ELIGIBLE = "Not Eligible";
        String IN_PROGRESS = "In Progress";
    }


    interface Map {
        int MAX_SELECT_ZOOM_LEVEL = 16;
        int CLICK_SELECT_RADIUS = 24;
        String NAME_PROPERTY = "name";
    }

    interface JsonForm {

        String ENCOUNTER_TYPE = "encounter_type";

        String SPRAY_STATUS = "sprayStatus";

        String TRAP_SET_DATE = "trap_start";

        String TRAP_FOLLOW_UP_DATE = "trap_end";

        String BUSINESS_STATUS = "business_status";

        String STRUCTURE_TYPE = "structureType";

        String HEAD_OF_HOUSEHOLD = "familyHeadName";

        String STRUCTURE_PROPERTIES_TYPE = "[structure_type]";

        String SPRAY_FORM = "json.form/spray_form.json";

        String THAILAND_MOSQUITO_COLLECTION_FORM = "json.form/thailand_mosquito_collection_form.json";

        String SPRAY_FORM_NAMIBIA = "json.form/namibia_spray_form.json";

        String SPRAY_FORM_BOTSWANA = "json.form/botswana_spray_form.json";

        String THAILAND_LARVAL_DIPPING_FORM = "json.form/thailand_larval_dipping_form.json";

        String ADD_STRUCTURE_FORM = "json.form/add_structure.json";

        String BEDNET_DISTRIBUTION_FORM = "json.form/bednet_distribution.json";

        String BLOOD_SCREENING_FORM = "json.form/blood_screening.json";

        String CASE_CONFIRMATION_FORM = "json.form/case_confirmation.json";

        String OPERATIONAL_AREA_TAG = "operational_area";

        String STRUCTURES_TAG = "structures";

        String NO_PADDING = "no_padding";

        String IS_MANDATORY = "is_mandatory";

        String SHORTENED_HINT = "shortened_hint";

        String HINT = "hint";
    }

    interface DateFormat {

        String EVENT_DATE_FORMAT_Z = "yyyy-MM-dd'T'HH:mm:ss.SSSZ";

        String EVENT_DATE_FORMAT_XXX = "yyyy-MM-dd'T'HH:mm:ss.SSSXXX";

        String CARD_VIEW_DATE_FORMAT = "dd MMM yyyy";
    }

    interface Action {
        String STRUCTURE_TASK_SYNCED = "reveal.STRUCTURE_TASK_SYNCED";
    }

    interface ECClientConfig {
        String NAMIBIA_EC_CLIENT_FIELDS = "ec_client_fields_namibia.json";
        String BOTSWANA_EC_CLIENT_FIELDS = "ec_client_fields_botswana.json";
    }


    interface StructureType {
        String RESIDENTIAL = "Residential Structure";

        String NON_RESIDENTIAL = "Non-Residential Structure";

        String MOSQUITO_COLLECTION_POINT = "Mosquito Collection Point";

        String LARVAL_BREEDING_SITE = "Larval Breeding Site";
    }

    interface TaskRegister {
        String VIEW_IDENTIFIER = "task_register";

        String INTERVENTION_TYPE = "intervention_type";

        String LAST_USER_LOCATION = "last_location";

    }

    interface DatabaseKeys {

        String TASK_TABLE = "task";

        String SPRAYED_STRUCTURES = "sprayed_structures";

        String STRUCTURES_TABLE = "structure";

        String STRUCTURE_NAME = "structure_name";

        String STRUCTURE_ID = "structure_id";

        String ID = "_id";

        String CODE = "code";

        String FOR = "for";

        String BUSINESS_STATUS = "business_status";

        String STATUS = "status";

        String FAMILY_NAME = "family_head_name";

        String SPRAY_STATUS = "spray_status";

        String LATITUDE = "latitude";

        String LONGITUDE = "longitude";

        String NAME = "name";

        String GROUPID = "group_id";

        String CAMPAIGN_ID = "campaign_id";

        String NOT_SRAYED_REASON = "not_sprayed_reason";

        String NOT_SRAYED_OTHER_REASON = "not_sprayed_other_reason";

        String OTHER = "other";


    }


}
