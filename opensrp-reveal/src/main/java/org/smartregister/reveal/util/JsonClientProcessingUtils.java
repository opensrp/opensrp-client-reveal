package org.smartregister.reveal.util;

import org.smartregister.clientandeventmodel.Event;
import org.smartregister.domain.Location;
import org.smartregister.domain.tag.FormTag;
import org.smartregister.family.FamilyLibrary;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.BuildConfig;

public class JsonClientProcessingUtils {

    public static Event tagSyncMetadata(AllSharedPreferences allSharedPreferences, Event event) {
        String providerId = allSharedPreferences.fetchRegisteredANM();
        event.setProviderId(providerId);
        event.setLocationId(getCurrentLocationID());
        event.setChildLocationId(allSharedPreferences.fetchCurrentLocality());
        event.setTeam(allSharedPreferences.fetchDefaultTeam(providerId));
        event.setTeamId(allSharedPreferences.fetchDefaultTeamId(providerId));

        event.setClientDatabaseVersion(BuildConfig.DATABASE_VERSION);
        event.setClientApplicationVersion(BuildConfig.VERSION_CODE);
        event.setClientApplicationVersionName(BuildConfig.VERSION_NAME);
        return event;
    }

    public static String getCurrentLocationID() {
        Location operationalArea = Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        return operationalArea == null ? "" : operationalArea.getId();
    }
}
