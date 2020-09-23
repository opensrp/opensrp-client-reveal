package org.smartregister.reveal.sync;

import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.util.Country;
import org.smartregister.sync.intent.SyncIntentService;

/**
 * Created by samuelgithengi on 10/17/19.
 */
public class RevealSyncIntentService extends SyncIntentService {

    @Override
    public int getEventPullLimit() {
        return (BuildConfig.BUILD_COUNTRY.equals(Country.NTD_COMMUNITY)) ? 250 : 500;
    }
}
