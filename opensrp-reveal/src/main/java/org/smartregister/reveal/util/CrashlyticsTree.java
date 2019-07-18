package org.smartregister.reveal.util;


import android.util.Log;

import com.crashlytics.android.Crashlytics;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.smartregister.reveal.application.RevealApplication;

import timber.log.Timber;

public class CrashlyticsTree extends Timber.Tree {

    private static final String CRASHLYTICS_KEY_PRIORITY = "priority";
    private static final String CRASHLYTICS_KEY_TAG = "tag";
    private static final String CRASHLYTICS_KEY_MESSAGE = "message";


    private String userName;

    @Override
    protected void log(int priority, @Nullable String tag, @NotNull String message, @Nullable Throwable t) {
        if (priority == Log.VERBOSE || priority == Log.DEBUG || priority == Log.INFO) {
            return;
        }

        if (userName == null) {
            userName = RevealApplication.getInstance().getContext().allSharedPreferences().fetchRegisteredANM();
        }

        Crashlytics.setInt(CRASHLYTICS_KEY_PRIORITY, priority);
        Crashlytics.setString(CRASHLYTICS_KEY_TAG, tag);
        Crashlytics.setString(CRASHLYTICS_KEY_MESSAGE, message);
        Crashlytics.setUserName(userName);
        if (t == null) {
            Crashlytics.logException(new Exception(message));
        } else {
            Crashlytics.logException(t);
        }
    }
}
