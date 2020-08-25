package org.smartregister.reveal.util;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.application.RevealApplication;

import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_DISTRICT;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_FACILITY;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_OPERATIONAL_AREA;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_OPERATIONAL_AREA_ID;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_PLAN;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_PLAN_ID;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_PROVINCE;
import static org.smartregister.reveal.util.Constants.Preferences.FACILITY_LEVEL;

/**
 * Created by samuelgithengi on 11/29/18.
 */
public class PreferencesUtil {

    private AllSharedPreferences allSharedPreferences;

    private static PreferencesUtil instance;

    private PreferencesUtil(AllSharedPreferences allSharedPreferences) {
        this.allSharedPreferences = allSharedPreferences;
    }


    public static PreferencesUtil getInstance() {
        if (instance == null) {
            instance = new PreferencesUtil(RevealApplication.getInstance().getContext().allSharedPreferences());
        }
        return instance;
    }

    public void setCurrentFacility(String facility) {
        allSharedPreferences.savePreference(CURRENT_FACILITY, facility);
    }

    public String getCurrentFacility() {
        return allSharedPreferences.getPreference(CURRENT_FACILITY);
    }

    public void setCurrentOperationalArea(String operationalArea) {
        allSharedPreferences.savePreference(CURRENT_OPERATIONAL_AREA, operationalArea);
        if (StringUtils.isNotBlank(operationalArea)) {
            allSharedPreferences.savePreference(CURRENT_OPERATIONAL_AREA_ID, Utils.getCurrentLocationId());
        }
    }

    public String getCurrentOperationalArea() {
        return allSharedPreferences.getPreference(CURRENT_OPERATIONAL_AREA);
    }

    public String getCurrentOperationalAreaId() {
        return allSharedPreferences.getPreference(CURRENT_OPERATIONAL_AREA_ID);
    }

    public void setCurrentDistrict(String district) {
        allSharedPreferences.savePreference(CURRENT_DISTRICT, district);
    }

    public String getCurrentDistrict() {
        return allSharedPreferences.getPreference(CURRENT_DISTRICT);
    }

    public void setCurrentProvince(String province) {
        allSharedPreferences.savePreference(CURRENT_PROVINCE, province);
    }

    public String getCurrentProvince() {
        return allSharedPreferences.getPreference(CURRENT_PROVINCE);
    }

    public void setCurrentPlan(String campaign) {
        allSharedPreferences.savePreference(CURRENT_PLAN, campaign);
    }

    public String getCurrentPlan() {
        return allSharedPreferences.getPreference(CURRENT_PLAN);
    }

    public void setCurrentPlanId(String campaignId) {
        allSharedPreferences.savePreference(CURRENT_PLAN_ID, campaignId);
    }

    public String getCurrentPlanId() {
        return allSharedPreferences.getPreference(CURRENT_PLAN_ID);
    }

    public String getPreferenceValue(String key) {
        return allSharedPreferences.getPreference(key);
    }

    public void setCurrentFacilityLevel(String facilityLevel) {
        allSharedPreferences.savePreference(FACILITY_LEVEL, facilityLevel);
    }


    public String getCurrentFacilityLevel() {
        return allSharedPreferences.getPreference(FACILITY_LEVEL);
    }

    public void setInterventionTypeForPlan(String planId, String interventionType) {
        allSharedPreferences.savePreference(planId, interventionType);
    }

    public String getInterventionTypeForPlan(String planId) {
        return allSharedPreferences.getPreference(planId);
    }

}
