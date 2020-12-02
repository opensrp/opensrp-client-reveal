package org.smartregister.reveal.shadow;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;
import org.smartregister.CoreLibrary;

/**
 * Created by samuelgithengi on 12/2/20.
 */
@Implements(CoreLibrary.class)
public class CoreLibraryShadow {

    @Implementation
    private static void checkPlatformMigrations() {
        //do nothing
    }
}
