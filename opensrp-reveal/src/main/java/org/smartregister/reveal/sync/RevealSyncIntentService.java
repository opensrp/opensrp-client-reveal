package org.smartregister.reveal.sync;

import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.sync.ClientProcessorForJava;
import org.smartregister.sync.intent.SyncIntentService;

/**
 * Created by samuelgithengi on 12/10/18.
 */
public class RevealSyncIntentService extends SyncIntentService {

    @Override
    protected ClientProcessorForJava getClientProcessor() {
        return RevealClientProcessor.getInstance(RevealApplication.getInstance().getApplicationContext());
    }
}
