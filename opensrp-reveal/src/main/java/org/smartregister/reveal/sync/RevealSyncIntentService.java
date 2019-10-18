package org.smartregister.reveal.sync;

import org.smartregister.sync.intent.SyncIntentService;

/**
 * Created by samuelgithengi on 10/17/19.
 */
public class RevealSyncIntentService extends SyncIntentService {

    @Override
    public int getEventPullLimit() {
        return 1000;
    }
}
