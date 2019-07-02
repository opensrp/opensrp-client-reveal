package org.smartregister.reveal.util;

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

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.domain.Location;
import org.smartregister.job.PullUniqueIdsServiceJob;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.job.LocationTaskServiceJob;
import org.smartregister.reveal.util.Constants.CONFIGURATION;
import org.smartregister.reveal.util.Constants.Tags;
import org.smartregister.util.Cache;
import org.smartregister.util.CacheableData;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;

import static org.smartregister.reveal.util.Constants.CONFIGURATION.KILOMETERS_PER_DEGREE_OF_LATITUDE_AT_EQUITOR;
import static org.smartregister.reveal.util.Constants.CONFIGURATION.KILOMETERS_PER_DEGREE_OF_LONGITUDE_AT_EQUITOR;
import static org.smartregister.reveal.util.Constants.CONFIGURATION.METERS_PER_KILOMETER;
import static org.smartregister.reveal.util.Constants.DateFormat.CARD_VIEW_DATE_FORMAT;
import static org.smartregister.reveal.util.Constants.Intervention.FI;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;

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
        return Float.valueOf(getGlobalConfig(CONFIGURATION.LOCATION_BUFFER_RADIUS_IN_METRES, BuildConfig.MY_LOCATION_BUFFER + ""));
    }


    public static Float getPixelsPerDPI(Resources resources) {
        return TypedValue.applyDimension(
                TypedValue.COMPLEX_UNIT_DIP,
                1,
                resources.getDisplayMetrics()
        );
    }

    public static int getInterventionLabel() {
        String plan = PreferencesUtil.getInstance().getCurrentPlan();
        String interventionType =  PreferencesUtil.getInstance().getInterventionTypeForPlan(plan);
        if (interventionType.equals(FI))
            return R.string.focus_investigation;
        else if (interventionType.equals(IRS))
            return R.string.irs;
        else
            return R.string.irs;
    }

    public static String getAge(String dob) {
        String dobString = org.smartregister.family.util.Utils.getDuration(dob);
        return dobString.contains("y") ? dobString.substring(0, dobString.indexOf("y")) : dobString;
    }

    public static Boolean getDrawOperationalAreaBoundaryAndLabel() {
        return Boolean.valueOf(getGlobalConfig(CONFIGURATION.DRAW_OPERATIONAL_AREA_BOUNDARY_AND_LABEL, CONFIGURATION.DEFAULT_DRAW_OPERATIONAL_AREA_BOUNDARY_AND_LABEL.toString()));
    }

    /**
     * Creates a circle using a GeoJSON polygon.
     * It's not strictly a circle but by increasing the number of sides on the polygon you can get pretty close to one.
     *
     * Adapted from https://stackoverflow.com/questions/37599561/drawing-a-circle-with-the-radius-in-miles-meters-with-mapbox-gl-js/39006388#39006388
     *
     * @param center - Coordinates for the center of the circle
     * @param radius - Radius of the circle in meters
     * @param points - Since this is a GeoJSON polygon, we need to have a large number of sides
     *               so that it gets as close as possible to being a circle
     * @return
     * @throws Exception
     */

    public static Feature createCircleFeature(LatLng center, Float radius, Float points) throws JSONException {
        Float radiusInKm = radius / METERS_PER_KILOMETER;

        JSONArray coordinates = new JSONArray();
        JSONArray coordinate = new JSONArray();
        JSONArray bufferArray = new JSONArray();
        double distanceX = radiusInKm / (KILOMETERS_PER_DEGREE_OF_LONGITUDE_AT_EQUITOR * Math.cos(center.getLatitude() * Math.PI / 180));
        double distanceY = radiusInKm / KILOMETERS_PER_DEGREE_OF_LATITUDE_AT_EQUITOR;

        double theta;
        double x;
        double y;
        for (int i = 0; i < points; i++) {
            theta = (i / points) * (2 * Math.PI);
            x = distanceX * Math.cos(theta);
            y = distanceY * Math.sin(theta);

            Double longitude = center.getLongitude() + x;
            Double latitude = center.getLatitude() + y;
            coordinate.put(longitude);
            coordinate.put(latitude);
            bufferArray.put(coordinate);
            coordinate = new JSONArray();
        }

        coordinates.put(bufferArray);

        JSONObject feature = new JSONObject();
        feature.put("type", "Feature");
        JSONObject geometry = new JSONObject();

        geometry.put("type", "Polygon");
        geometry.put("coordinates", coordinates);
        feature.put("geometry", geometry);

        return Feature.fromJson(feature.toString());
    }

    /**
     *
     * Determines whether a structure is a residence based on the Task Code value
     * @param taskCode
     * @return isResidentialStructure
     */
    public static boolean isResidentialStructure(String taskCode) {
        if (StringUtils.isEmpty(taskCode)) {
            return false;
        }
        return !(MOSQUITO_COLLECTION.equals(taskCode) || LARVAL_DIPPING.equals(taskCode));
    }
}
