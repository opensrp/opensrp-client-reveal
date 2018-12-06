package org.smartregister.reveal.util;

public interface Constants {

    String VIEW_CONFIGURATION_PREFIX = "ViewConfiguration_";

    String FILTER_TEAM_ID = "teamId";

    interface CONFIGURATION {
        String LOGIN = "login";
    }

    interface Preferences {
        String CURRENT_OPERATIONAL_AREA = "CURRENT_OPERATIONAL_AREA";
        String CURRENT_FACILITY = "CURRENT_FACILITY";
        String CURRENT_DISTRICT = "CURRENT_DISTRICT";
        String CURRENT_CAMPAIGN = "CURRENT_CAMPAIGN";
        String CURRENT_CAMPAIGN_ID = "CURRENT_CAMPAIGN_ID";
    }

    interface Tags {
        String COUNTRY = "Country";
        String PROVINCE = "Province";
        String DISTRICT = "District";
        String HEALTH_CENTER = "Rural Health Centre";
        String OPERATIONAL_AREA = "Operational Area";
    }

    interface Properties {
        String TASK_IDENTIFIER = "taskIdentifier";
        String TASK_BUSINESS_STATUS = "taskBusinessStatus";
        String TASK_STATUS = "taskStatus";
    }


    interface GeoJSON {
        String TYPE = "type";
        String FEATURE_COLLECTION = "FeatureCollection";
        String FEATURES = "features";
    }


}
