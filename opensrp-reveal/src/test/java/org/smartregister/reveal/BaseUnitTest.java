package org.smartregister.reveal;

import android.os.Build;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.joda.time.DateTime;
import org.junit.Rule;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.annotation.Config;
import org.smartregister.reveal.shadow.AsyncTaskShadow;
import org.smartregister.reveal.shadow.CloudantDataHandlerShadowUtils;
import org.smartregister.reveal.shadow.CredentialsHelperShadow;
import org.smartregister.reveal.shadow.CustomFontTextViewShadow;
import org.smartregister.reveal.shadow.GeoJsonSourceShadow;
import org.smartregister.reveal.shadow.KujakuMapViewShadow;
import org.smartregister.reveal.shadow.LayerShadow;
import org.smartregister.reveal.shadow.LineLayerShadow;
import org.smartregister.reveal.shadow.MapViewShadow;
import org.smartregister.reveal.shadow.OfflineManagerShadow;
import org.smartregister.reveal.shadow.RevealMapViewShadow;
import org.smartregister.reveal.shadow.SourceShadow;
import org.smartregister.reveal.shadow.SymbolLayerShadow;
import org.smartregister.util.DateTimeTypeConverter;


@RunWith(RobolectricTestRunner.class)
@Config(application = TestRevealApplication.class, shadows = {CustomFontTextViewShadow.class,
        MapViewShadow.class, KujakuMapViewShadow.class, RevealMapViewShadow.class,
        LayerShadow.class, SymbolLayerShadow.class, LineLayerShadow.class,
        GeoJsonSourceShadow.class, SourceShadow.class, OfflineManagerShadow.class,
        AsyncTaskShadow.class, CloudantDataHandlerShadowUtils.class, CredentialsHelperShadow.class}, sdk = Build.VERSION_CODES.P)
public abstract class BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    protected static final String DUMMY_USERNAME = "myusername";
    protected static final char[] DUMMY_PASSWORD = "mypassword".toCharArray();

    protected final int ASYNC_TIMEOUT = 5000;

    protected static Gson taskGson = new GsonBuilder().registerTypeAdapter(DateTime.class, new DateTimeTypeConverter("yyyy-MM-dd'T'HHmm"))
            .serializeNulls().create();

    protected static String getString(int stringResourceId) {
        return RuntimeEnvironment.application.getResources().getString(stringResourceId);
    }
}
