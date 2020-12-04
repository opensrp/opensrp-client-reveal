package org.smartregister.reveal.shadow;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;
import org.smartregister.util.CredentialsHelper;

/**
 * Created by samuelgithengi on 12/4/20.
 */
@Implements(CredentialsHelper.class)
public class CredentialsHelperShadow {

    @Implementation
    public static boolean shouldMigrate() {
        return false;
    }
}
