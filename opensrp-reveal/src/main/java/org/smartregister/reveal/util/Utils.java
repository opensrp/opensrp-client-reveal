package org.smartregister.reveal.util;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.os.Build;
import android.preference.PreferenceManager;
import android.text.TextUtils;
import android.util.DisplayMetrics;
import android.util.Log;
import android.util.TypedValue;
import android.widget.Toast;

import com.google.common.base.Splitter;
import com.google.common.collect.Iterables;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.smartregister.immunization.domain.Vaccine;
import org.smartregister.immunization.repository.VaccineRepository;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.util.DateUtil;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Locale;
import java.util.Map;

/**
 * Created by ndegwamartin on 14/03/2018.
 */

public class Utils {

    public static final int DOSE_EXPIRY_WINDOW_DAYS = 10;
    public static final ArrayList<String> ALLOWED_LEVELS;
    public static final String DEFAULT_LOCATION_LEVEL = "Health Facility";
    public static final String SCHOOL = "School";
    private static final String TAG = Utils.class.getCanonicalName();
    private static final SimpleDateFormat DB_DF = new SimpleDateFormat("yyyy-MM-dd");

    static {
        ALLOWED_LEVELS = new ArrayList<>();
        ALLOWED_LEVELS.add(DEFAULT_LOCATION_LEVEL);
        ALLOWED_LEVELS.add(SCHOOL);
    }


    public static void addVaccine(VaccineRepository vaccineRepository, Vaccine vaccine) {
        try {
            if (vaccineRepository == null || vaccine == null) {
                return;
            }
            vaccineRepository.add(vaccine);
        } catch (Exception e) {
            Log.e(Utils.class.getCanonicalName(), Log.getStackTraceString(e));
        }
    }

    public static void showToast(Context context, String message) {
        Toast.makeText(context, message, Toast.LENGTH_LONG).show();

    }

    public static void showShortToast(Context context, String message) {
        Toast.makeText(context, message, Toast.LENGTH_SHORT).show();

    }

    public static void saveLanguage(String language) {
        AllSharedPreferences allSharedPreferences = new AllSharedPreferences(PreferenceManager.getDefaultSharedPreferences(RevealApplication.getInstance().getApplicationContext()));
        allSharedPreferences.saveLanguagePreference(language);
        setLocale(new Locale(language));


    }


    public static String getLanguage() {
        AllSharedPreferences allSharedPreferences = new AllSharedPreferences(PreferenceManager.getDefaultSharedPreferences(RevealApplication.getInstance().getApplicationContext()));
        return allSharedPreferences.fetchLanguagePreference();
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


    public static <T, E> T getKeyByValue(Map<T, E> map, E value) {
        for (Map.Entry<T, E> entry : map.entrySet()) {
            if (value.equals(entry.getValue())) {
                return entry.getKey();
            }
        }
        return null;
    }

    public static int getTokenStringResourceId(Context context, String token) {
        return context.getResources().getIdentifier(token, "string", context.getPackageName());
    }

    public static int getLayoutIdentifierResourceId(Context context, String token) {
        return context.getResources().getIdentifier(token, "id", context.getPackageName());
    }

    public static String readPrefString(Context context, final String key, String defaultValue) {
        SharedPreferences pref = PreferenceManager.getDefaultSharedPreferences(context);
        return pref.getString(key, defaultValue);
    }

    public static void writePrefString(Context context, final String key, final String value) {
        SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(context);
        SharedPreferences.Editor editor = settings.edit();
        editor.putString(key, value);
        editor.commit();
    }

    public static Date dobStringToDate(String dobString) {
        DateTime dateTime = dobStringToDateTime(dobString);
        if (dateTime != null) {
            return dateTime.toDate();
        }
        return null;
    }

    public static DateTime dobStringToDateTime(String dobString) {
        try {
            if (StringUtils.isBlank(dobString)) {
                return null;
            }
            return new DateTime(dobString);

        } catch (Exception e) {
            return null;
        }
    }

    public static String formatDate(String date) {
        return StringUtils.isNotEmpty(date) ? new DateTime(date).toString("dd/MM/yy") : date;
    }

    public static String getDuration(String date) {
        DateTime duration;
        if (StringUtils.isNotBlank(date)) {
            try {
                duration = new DateTime(date);
                return DateUtil.getDuration(duration);
            } catch (Exception e) {
                Log.e(TAG, e.toString(), e);
            }
        }
        return "";
    }


    public static String convertDateFormat(Date date, SimpleDateFormat formatter) {

        return formatter.format(date);
    }

    public static String getTodaysDate() {
        return convertDateFormat(Calendar.getInstance().getTime(), DB_DF);
    }

    public static int convertDpToPx(Context context, int dp) {
        Resources r = context.getResources();
        float px = TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, dp, r.getDisplayMetrics());
        return Math.round(px);
    }

    public static void putAll(Map<String, String> map, Map<String, String> extend) {
        Collection<String> values = extend.values();
        while (true) {
            if (!(values.remove(null))) break;
        }
        map.putAll(extend);
    }

    public static String getFormattedAgeString(String dobString) {
        String formattedAge = "";
        if (!TextUtils.isEmpty(dobString)) {
            DateTime dateTime = new DateTime(dobString);
            Date dob = dateTime.toDate();
            long timeDiff = Calendar.getInstance().getTimeInMillis() - dob.getTime();

            if (timeDiff >= 0) {
                formattedAge = DateUtil.getDuration(timeDiff);
            }
        }
        return formattedAge.contains("y") ? formattedAge.substring(0, formattedAge.indexOf('y')) : formattedAge;
    }

    public static String getFormattedPhoneNumber(String phoneNumber_) {
        if (phoneNumber_ != null) {
            String phoneNumber = phoneNumber_.startsWith("0") ? phoneNumber_.substring(1) : phoneNumber_;
            String[] tokens = Iterables.toArray(Splitter.fixedLength(3).split(phoneNumber), String.class);
            return "256-" + StringUtils.join(tokens, "-");
        } else {
            return "";
        }

    }


}
