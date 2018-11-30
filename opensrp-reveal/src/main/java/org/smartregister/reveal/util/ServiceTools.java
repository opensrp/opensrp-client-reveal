package org.smartregister.reveal.util;

import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;

import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.sync.intent.SyncIntentService;

import java.util.List;

/**
 * Created by ndegwamartin on 15/03/2018.
 */

public class ServiceTools {

    private static boolean isServiceRunning(Class<?> serviceClass) {
        final ActivityManager activityManager = (ActivityManager) RevealApplication.getInstance().getSystemService(Context.ACTIVITY_SERVICE);
        final List<ActivityManager.RunningServiceInfo> services = activityManager.getRunningServices(Integer.MAX_VALUE);

        for (ActivityManager.RunningServiceInfo service : services) {
            if (serviceClass.getName().equals(service.service.getClassName())) {
                return true;
            }
        }
        return false;
    }

    public static void startService(Context context, Class<?> serviceClass) {
        if (context == null || serviceClass == null) {
            return;
        }

        if (!isServiceRunning(serviceClass)) {
            Intent intent = new Intent(context, serviceClass);
            RevealApplication.getInstance().startService(intent);
        }

    }

    public static void startSyncService(Context context) {
        if (context == null) {
            return;
        }

        Intent intent = new Intent(context, SyncIntentService.class);
        context.startService(intent);

    }

    public static void startService(Context context, Class serviceClass, boolean wakeup) {
        if (context == null || serviceClass == null) {
            return;
        }

        Intent intent = new Intent(context, serviceClass);

        context.startService(intent);

    }
}
