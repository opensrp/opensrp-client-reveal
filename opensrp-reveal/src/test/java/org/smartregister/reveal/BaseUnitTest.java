package org.smartregister.reveal;

import android.os.Build;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.joda.time.DateTime;
import org.junit.runner.RunWith;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.modules.junit4.PowerMockRunnerDelegate;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.annotation.Config;
import org.smartregister.reveal.shadow.CustomFontTextViewShadow;
import org.smartregister.reveal.shadow.OfflineManagerShadow;
import org.smartregister.reveal.shadow.RevealMapViewShadow;
import org.smartregister.reveal.shadow.KujakuMapViewShadow;
import org.smartregister.reveal.shadow.MapViewShadow;
import org.smartregister.util.DateTimeTypeConverter;


@RunWith(RobolectricTestRunner.class)
@PowerMockRunnerDelegate(RobolectricTestRunner.class)
@Config(application = TestRevealApplication.class, shadows = {CustomFontTextViewShadow.class, MapViewShadow.class, KujakuMapViewShadow.class, RevealMapViewShadow.class, OfflineManagerShadow.class}, sdk = Build.VERSION_CODES.P)
@PowerMockIgnore({"org.mockito.*", "org.robolectric.*", "android.*"})
public abstract class BaseUnitTest {

    protected static final String DUMMY_USERNAME = "myusername";
    protected static final String DUMMY_PASSWORD = "mypassword";

    protected final int ASYNC_TIMEOUT = 2000;

    protected static Gson taskGson = new GsonBuilder().registerTypeAdapter(DateTime.class, new DateTimeTypeConverter("yyyy-MM-dd'T'HHmm"))
            .serializeNulls().create();

    protected static String getString(int stringResourceId) {
        return RuntimeEnvironment.application.getResources().getString(stringResourceId);
    }
}
