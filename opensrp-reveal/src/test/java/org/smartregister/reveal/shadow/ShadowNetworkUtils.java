package org.smartregister.reveal.shadow;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;
import org.smartregister.util.NetworkUtils;

/**
 * Created by samuelgithengi on 2/9/21.
 */
@Implements(NetworkUtils.class)
public class ShadowNetworkUtils {

    public static boolean isNetworkAvailable;

    @Implementation
    public static boolean isNetworkAvailable() {
        return isNetworkAvailable;
    }

    public static void setIsNetworkAvailable(boolean isNetworkAvailable) {
        ShadowNetworkUtils.isNetworkAvailable = isNetworkAvailable;
    }
}
