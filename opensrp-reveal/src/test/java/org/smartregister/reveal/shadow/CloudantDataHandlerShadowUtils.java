package org.smartregister.reveal.shadow;

import android.content.Context;

import org.mockito.Mockito;
import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;
import org.smartregister.sync.CloudantDataHandler;

/**
 * Created by Richard Kareko on 5/29/20.
 */

@Implements(CloudantDataHandler.class)
public class CloudantDataHandlerShadowUtils {

    @Implementation
    public static CloudantDataHandler getInstance(Context context) {
        return Mockito.mock(CloudantDataHandler.class);
    }
}
