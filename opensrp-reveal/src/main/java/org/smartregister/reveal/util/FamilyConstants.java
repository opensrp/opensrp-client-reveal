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
        String FAMILY_REGISTRATION = "Family_Registration";
        String FAMILY_REGISTRATION_INELIGIBLE = "Family_Registration_Ineligible";
        String FAMILY_MEMBER_REGISTRATION = "Family_Member_Registration";

        String UPDATE_FAMILY_REGISTRATION = "Update_Family_Registration";
        String UPDATE_FAMILY_MEMBER_REGISTRATION = "Update_Family_Member_Registration";

        String ARCHIVE_FAMILY = "Archive_Family";

        String ARCHIVE_FAMILY_MEMBER = "Archive_Family_Member";
    }

    interface JSON_FORM {
        String FAMILY_REGISTER = "family_register";
        String FAMILY_UPDATE = "family_update";
        String FAMILY_MEMBER_REGISTER = "family_member_register";

        String THAILAND_FAMILY_REGISTER = "thailand_family_register";
        String THAILAND_FAMILY_UPDATE = "thailand_family_update";
        String THAILAND_FAMILY_MEMBER_REGISTER = "thailand_family_member_register";

        String ZAMBIA_FAMILY_REGISTER = "zambia_family_register";
        String ZAMBIA_FAMILY_UPDATE = "zambia_family_update";
        String ZAMBIA_FAMILY_MEMBER_REGISTER = "zambia_family_member_register";

        String REFAPP_FAMILY_REGISTER = "refapp_family_register";
        String REFAPP_FAMILY_UPDATE = "refapp_family_update";
        String REFAPP_FAMILY_MEMBER_REGISTER = "refapp_family_member_register";
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

    interface DatabaseKeys {
        String FAMILY_NAME = "fam_name";
        String OLD_FAMILY_NAME = "old_fam_name";
        String HOUSE_NUMBER = "house_number";
        String AGE = "age";
        String NATIONAL_ID = "national_id";
        String CITIZENSHIP = "citizenship";
        String OCCUPATION = "occupation";
        String SLEEPS_OUTDOORS = "sleeps_outdoors";
        String PHONE_NUMBER = "phone_number";
        String IS_FAMILY_HEAD = "is_family_head";
        String PREFIX = "prefix";
    }

    interface FormKeys {
        String SEX = "sex";
        String SAME_AS_FAM_NAME = "same_as_fam_name";
        String SAME_AS_FAM_FIRST_NAME = "first_name_as_fam_name";
        String SURNAME = "surname";
        String FIRST_NAME = "first_name";
    }


}
