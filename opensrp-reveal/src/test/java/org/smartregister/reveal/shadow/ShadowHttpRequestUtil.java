package org.smartregister.reveal.shadow;

import com.mapbox.mapboxsdk.module.http.HttpRequestUtil;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;

import javax.annotation.Nullable;

import okhttp3.OkHttpClient;

/**
 * Created by Richard Kareko on 8/7/21.
 */

@Implements(HttpRequestUtil.class)
public class ShadowHttpRequestUtil {

    @Implementation
    public static void setOkHttpClient(@Nullable OkHttpClient client) {
        //Do nothing
    }
}
