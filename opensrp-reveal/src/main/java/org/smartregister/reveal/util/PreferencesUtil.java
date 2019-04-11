package org.smartregister.reveal.util;

import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.application.RevealApplication;

import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_CAMPAIGN;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_CAMPAIGN_ID;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_DISTRICT;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_FACILITY;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_OPERATIONAL_AREA;
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
    }

    public String getCurrentOperationalArea() {
        return allSharedPreferences.getPreference(CURRENT_OPERATIONAL_AREA);
    }

    public void setCurrentDistrict(String district) {
        allSharedPreferences.savePreference(CURRENT_DISTRICT, district);
    }

    public String getCurrentDistrict() {
        return allSharedPreferences.getPreference(CURRENT_DISTRICT);
    }

    public void setCurrentCampaign(String campaign) {
        allSharedPreferences.savePreference(CURRENT_CAMPAIGN, campaign);
    }

    public String getCurrentCampaign() {
        return allSharedPreferences.getPreference(CURRENT_CAMPAIGN);
    }

    public void setCurrentCampaignId(String campaignId) {
        allSharedPreferences.savePreference(CURRENT_CAMPAIGN_ID, campaignId);
    }

    public String getCurrentCampaignId() {
        return allSharedPreferences.getPreference(CURRENT_CAMPAIGN_ID);
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


}
