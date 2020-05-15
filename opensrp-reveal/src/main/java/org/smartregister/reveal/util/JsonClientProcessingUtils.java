package org.smartregister.reveal.util;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.domain.Location;
import org.smartregister.domain.tag.FormTag;
import org.smartregister.family.FamilyLibrary;
import org.smartregister.repository.AllSharedPreferences;

import timber.log.Timber;

public class JsonClientProcessingUtils {

    public static FormTag formTag(AllSharedPreferences allSharedPreferences) {
        FormTag formTag = new FormTag();
        formTag.providerId = allSharedPreferences.fetchRegisteredANM();
        formTag.appVersion = FamilyLibrary.getInstance().getApplicationVersion();
        formTag.databaseVersion = FamilyLibrary.getInstance().getDatabaseVersion();
        return formTag;
    }

    public static Event tagSyncMetadata(AllSharedPreferences allSharedPreferences, Event event) {
        String providerId = allSharedPreferences.fetchRegisteredANM();
        event.setProviderId(providerId);
        event.setLocationId(getCurrentLocationID(allSharedPreferences));
        event.setChildLocationId(allSharedPreferences.fetchCurrentLocality());
        event.setTeam(allSharedPreferences.fetchDefaultTeam(providerId));
        event.setTeamId(allSharedPreferences.fetchDefaultTeamId(providerId));

        event.setClientDatabaseVersion(FamilyLibrary.getInstance().getDatabaseVersion());
        event.setClientApplicationVersion(FamilyLibrary.getInstance().getApplicationVersion());
        return event;
    }

    public static String getCurrentLocationID(AllSharedPreferences allSharedPreferences) {
        Location operationalArea = Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        String userLocationId = null;
        try {
            userLocationId = operationalArea.getProperties().getUid();
            if (userLocationId == null)
                userLocationId = operationalArea.getProperties().getCustomProperties().get("OpenMRS_Id");
        } catch (Exception e) {
            Timber.e(e);
        }

        if (userLocationId == null) {
            String providerId = allSharedPreferences.fetchRegisteredANM();
            userLocationId = allSharedPreferences.fetchUserLocalityId(providerId);
            if (StringUtils.isBlank(userLocationId)) {
                userLocationId = allSharedPreferences.fetchDefaultLocalityId(providerId);
            }
        }
        return userLocationId;
    }
}
