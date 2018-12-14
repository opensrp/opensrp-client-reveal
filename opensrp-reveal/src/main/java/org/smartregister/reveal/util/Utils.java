package org.smartregister.reveal.util;

import android.content.res.Configuration;
import android.content.res.Resources;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.annotation.StringRes;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.ForegroundColorSpan;
import android.util.DisplayMetrics;
import android.widget.TextView;

import com.google.gson.JsonElement;
import com.mapbox.geojson.Feature;

import org.smartregister.job.CampaignServiceJob;
import org.smartregister.job.LocationStructureServiceJob;
import org.smartregister.job.SyncServiceJob;
import org.smartregister.job.SyncTaskServiceJob;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;

import java.util.ArrayList;
import java.util.Locale;

import static org.smartregister.reveal.util.Constants.Properties.LOCATION_UUID;

public class Utils {

    public static final ArrayList<String> ALLOWED_LEVELS;
    public static final String DEFAULT_LOCATION_LEVEL = "Rural Health Centre";
    public static final String OPERATIONAL_AREA = "Operational Area";
    public static final String REVEAL_PROJECT = "reveal";

    static {
        ALLOWED_LEVELS = new ArrayList<>();
        ALLOWED_LEVELS.add(DEFAULT_LOCATION_LEVEL);
        ALLOWED_LEVELS.add(OPERATIONAL_AREA);
        ALLOWED_LEVELS.add(REVEAL_PROJECT);
    }

    public static void saveLanguage(String language) {
        AllSharedPreferences allSharedPreferences = new AllSharedPreferences(PreferenceManager.getDefaultSharedPreferences(RevealApplication.getInstance().getApplicationContext()));
        allSharedPreferences.saveLanguagePreference(language);
        setLocale(new Locale(language));
    }

    public static void setLocale(Locale locale) {
        Resources resources = RevealApplication.getInstance().getApplicationContext().getResources();
        Configuration configuration = resources.getConfiguration();
        DisplayMetrics displayMetrics = resources.getDisplayMetrics();
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) {
            configuration.setLocale(locale);
            RevealApplication.getInstance().getApplicationContext().createConfigurationContext(configuration);
        } else {
            configuration.locale = locale;
            resources.updateConfiguration(configuration, displayMetrics);
        }
    }


    public static void setTextViewText(@NonNull TextView textView, @NonNull @StringRes Integer labelResource, String value) {
        SpannableStringBuilder builder = new SpannableStringBuilder();
        ForegroundColorSpan blackSpan = new ForegroundColorSpan(textView.getResources().getColor(R.color.text_black));
        builder.append(textView.getResources().getString(labelResource)).append(" ");
        int start = builder.length();
        builder.append(value).setSpan(blackSpan, start, builder.length(), Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        textView.setText(builder);
    }

    public static String getPropertyValue(Feature feature, String propertyKey) {
        JsonElement featureProperty = feature.getProperty(propertyKey);
        return featureProperty == null ? null : featureProperty.getAsString();
    }

    public static void startImmediateSync() {
        CampaignServiceJob.scheduleJobImmediately(CampaignServiceJob.TAG);

        LocationStructureServiceJob.scheduleJobImmediately(LocationStructureServiceJob.TAG);

        Handler mainThreadHandler = new Handler(Looper.getMainLooper());
        mainThreadHandler.postDelayed(new Runnable() {
            @Override
            public void run() {
                SyncTaskServiceJob.scheduleJobImmediately(SyncTaskServiceJob.TAG);

                SyncServiceJob.scheduleJobImmediately(SyncServiceJob.TAG);
            }
        }, 5000);

    }

}
