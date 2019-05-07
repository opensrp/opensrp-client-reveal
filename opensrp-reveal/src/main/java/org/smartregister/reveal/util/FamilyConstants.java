package org.smartregister.reveal.util;

/**
 * Created by samuelgithengi on 2/8/19.
 */
public interface FamilyConstants {


    interface CONFIGURATION {
        String FAMILY_REGISTER = "family_register";
        String FAMILY_MEMBER_REGISTER = "family_member_register";
        String UNIQUE_ID_KEY = "opensrp_id";

    }

    interface EventType {
        String FAMILY_REGISTRATION = "Family Registration";
        String FAMILY_MEMBER_REGISTRATION = "Family Member Registration";

        String UPDATE_FAMILY_REGISTRATION = "Update Family Registration";
        String UPDATE_FAMILY_MEMBER_REGISTRATION = "Update Family Member Registration";
    }

    interface JSON_FORM {
        String FAMILY_REGISTER = "family_register";
        String FAMILY_MEMBER_REGISTER = "family_member_register";
    }

    interface RELATIONSHIP {
        String FAMILY = "family";
        String FAMILY_HEAD = "family_head";
        String PRIMARY_CAREGIVER = "primary_caregiver";
        String RESIDENCE = "residence";

    }

    interface TABLE_NAME {
        String FAMILY = "ec_family";
        String FAMILY_MEMBER = "ec_family_member";
    }

    interface Intent {
        String START_REGISTRATION = "START_REGISTRATION";
    }


}
