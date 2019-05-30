package org.smartregister.reveal.util;

import android.content.Context;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.os.Build;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.annotation.StringRes;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.ForegroundColorSpan;
import android.util.DisplayMetrics;
import android.util.TypedValue;
import android.widget.TextView;

import com.google.gson.JsonElement;
import com.mapbox.geojson.Feature;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;

import org.smartregister.domain.Location;
import org.smartregister.job.PullUniqueIdsServiceJob;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.job.LocationTaskServiceJob;
import org.smartregister.reveal.util.Constants.CONFIGURATION;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Constants.Tags;
import org.smartregister.util.Cache;
import org.smartregister.util.CacheableData;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;

import static org.smartregister.reveal.util.Constants.DateFormat.CARD_VIEW_DATE_FORMAT;

public class Utils {

    public static final ArrayList<String> ALLOWED_LEVELS;
    public static final String DEFAULT_LOCATION_LEVEL = Tags.HEALTH_CENTER;
    public static final String REVEAL_PROJECT = "reveal";

    private static Cache<Location> cache = new Cache<>();

    static {
        ALLOWED_LEVELS = new ArrayList<>();
        ALLOWED_LEVELS.add(DEFAULT_LOCATION_LEVEL);
        ALLOWED_LEVELS.add(Tags.OPERATIONAL_AREA);
        ALLOWED_LEVELS.add(Tags.CANTON);
        ALLOWED_LEVELS.add(Tags.VILLAGE);
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
        LocationTaskServiceJob.scheduleJobImmediately(LocationTaskServiceJob.TAG);
        PullUniqueIdsServiceJob.scheduleJobImmediately(PullUniqueIdsServiceJob.TAG);
    }


    public static Location getOperationalAreaLocation(String operationalArea) {
        return cache.get(operationalArea, new CacheableData<Location>() {
            @Override
            public Location fetch() {
                return RevealApplication.getInstance().getLocationRepository().getLocationByName(PreferencesUtil.getInstance().getCurrentOperationalArea());
            }
        });
    }

    public static String formatDate(String date, String dateFormat) throws Exception {
        DateFormat sdf = new SimpleDateFormat(dateFormat, Locale.getDefault());
        Date originalDate = sdf.parse(date);

        sdf = new SimpleDateFormat(CARD_VIEW_DATE_FORMAT, Locale.getDefault());

        return sdf.format(originalDate);

    }

    public static String getGlobalConfig(String key, String defaultValue) {
        String val = RevealApplication.getInstance().getGlobalConfigs().get(key);
        return val == null ? defaultValue : val;
    }

    public static Float getLocationBuffer() {
        return Float.valueOf(getGlobalConfig(CONFIGURATION.LOCATION_BUFFER_RADIUS_IN_METRES, CONFIGURATION.DEFAULT_LOCATION_BUFFER_RADIUS_IN_METRES.toString()));
    }

    public static int getInterventionLabel() {
        String plan = PreferencesUtil.getInstance().getCurrentPlan();
        if (plan.toUpperCase().contains(Intervention.FOCUS))
            return R.string.focus_investigation;
        else if (plan.toUpperCase().contains(Intervention.IRS))
            return R.string.irs;
        else
            return R.string.irs;
    }

    public static float calculateZoomLevelRadius(@NonNull final MapboxMap mapboxMap, double latitude, float radius, Context context) {
        double metersPerDip = mapboxMap.getProjection().getMetersPerPixelAtLatitude(latitude);
        double metersPerPixel;

        float dip = 1.0f;
        Resources r = context.getResources();
        float dpToPixelRatio = TypedValue.applyDimension(
                TypedValue.COMPLEX_UNIT_DIP,
                dip,
                r.getDisplayMetrics()
        );

        // Apparently from https://github.com/mapbox/mapbox-gl-native/issues/8990 seems like the getMetersPerPixelAtLatitude()
        // function returns meters per dip hence we have to factor that in when calculating the zoom level radius
        metersPerPixel = metersPerDip * dpToPixelRatio;

        return (float) (radius / metersPerPixel);
    }

    public static String getAge(String dob) {
        String dobString = org.smartregister.family.util.Utils.getDuration(dob);
        return dobString.contains("y") ? dobString.substring(0, dobString.indexOf("y")) : dobString;
    }

    public static Boolean getDrawOperationalAreaBoundaryAndLabel() {
        return Boolean.valueOf(getGlobalConfig(CONFIGURATION.DRAW_OPERATIONAL_AREA_BOUNDARY_AND_LABEL, CONFIGURATION.DEFAULT_DRAW_OPERATIONAL_AREA_BOUNDARY_AND_LABEL.toString()));
    }
}
