package org.smartregister.reveal.application;

import android.content.Intent;
import android.support.annotation.NonNull;
import android.util.Log;

import com.crashlytics.android.Crashlytics;
import com.crashlytics.android.core.CrashlyticsCore;
import com.evernote.android.job.JobManager;
import com.mapbox.mapboxsdk.Mapbox;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.Context;
import org.smartregister.CoreLibrary;
import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.configurableviews.helper.JsonSpecHelper;
import org.smartregister.domain.Setting;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.receiver.SyncStatusBroadcastReceiver;
import org.smartregister.repository.AllSettings;
import org.smartregister.repository.CampaignRepository;
import org.smartregister.repository.LocationRepository;
import org.smartregister.repository.Repository;
import org.smartregister.repository.StructureRepository;
import org.smartregister.repository.TaskNotesRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.activity.LoginActivity;
import org.smartregister.reveal.job.RevealJobCreator;
import org.smartregister.reveal.repository.RevealRepository;
import org.smartregister.reveal.util.RevealSyncConfiguration;
import org.smartregister.reveal.util.Utils;
import org.smartregister.sync.DrishtiSyncScheduler;
import org.smartregister.view.activity.DrishtiApplication;
import org.smartregister.view.receiver.TimeChangedBroadcastReceiver;

import java.util.HashMap;
import java.util.Map;

import io.fabric.sdk.android.Fabric;

import static org.smartregister.reveal.util.Constants.CONFIGURATION.GLOBAL_CONFIGS;
import static org.smartregister.reveal.util.Constants.CONFIGURATION.KEY;
import static org.smartregister.reveal.util.Constants.CONFIGURATION.VALUE;
import static org.smartregister.util.Log.logError;
import static org.smartregister.util.Log.logInfo;

public class RevealApplication extends DrishtiApplication implements TimeChangedBroadcastReceiver.OnTimeChangedListener {

    private static final String TAG = RevealApplication.class.getCanonicalName();
    private JsonSpecHelper jsonSpecHelper;
    private String password;

    private CampaignRepository campaignRepository;
    private TaskRepository taskRepository;
    private StructureRepository structureRepository;
    private LocationRepository locationRepository;

    private Map<String, String> globalConfigs;

    public static synchronized RevealApplication getInstance() {
        return (RevealApplication) mInstance;
    }

    public static JsonSpecHelper getJsonSpecHelper() {
        return getInstance().jsonSpecHelper;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mInstance = this;
        context = Context.getInstance();
        context.updateApplicationContext(getApplicationContext());
        // Initialize Modules
        Fabric.with(this, new Crashlytics.Builder().core(new CrashlyticsCore.Builder().disabled(BuildConfig.DEBUG).build()).build());
        CoreLibrary.init(context, new RevealSyncConfiguration());
        ConfigurableViewsLibrary.init(context, getRepository());
        LocationHelper.init(Utils.ALLOWED_LEVELS, Utils.DEFAULT_LOCATION_LEVEL);

        SyncStatusBroadcastReceiver.init(this);

        jsonSpecHelper = new JsonSpecHelper(this);
        globalConfigs = new HashMap<>();

        Mapbox.getInstance(getApplicationContext(), BuildConfig.MAPBOX_SDK_ACCESS_TOKEN);

        try {
            Utils.saveLanguage("en");
        } catch (Exception e) {
            Log.e(TAG, e.getMessage());
        }

        //init Job Manager
        JobManager.create(this).addJobCreator(new RevealJobCreator());
    }

    @Override
    public Repository getRepository() {
        try {
            if (repository == null) {
                repository = new RevealRepository(getInstance().getApplicationContext(), context);
            }
        } catch (UnsatisfiedLinkError e) {
            logError("Error on getRepository: " + e);

        }
        return repository;
    }

    public String getPassword() {
        if (password == null) {
            String username = getContext().userService().getAllSharedPreferences().fetchRegisteredANM();
            password = getContext().userService().getGroupId(username);
        }
        return password;
    }

    @Override
    public void logoutCurrentUser() {
        Intent intent = new Intent(getApplicationContext(), LoginActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.addCategory(Intent.CATEGORY_HOME);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        getApplicationContext().startActivity(intent);
        context.userService().logoutSession();
    }

    public Context getContext() {
        return context;
    }

    protected void cleanUpSyncState() {
        try {
            DrishtiSyncScheduler.stop(getApplicationContext());
            context.allSharedPreferences().saveIsSyncInProgress(false);
        } catch (Exception e) {
            Log.e(TAG, e.getMessage());
        }
    }

    @Override
    public void onTerminate() {
        logInfo("Application is terminating. Stopping Sync scheduler and resetting isSyncInProgress setting.");
        cleanUpSyncState();
        TimeChangedBroadcastReceiver.destroy(this);
        SyncStatusBroadcastReceiver.destroy(this);
        super.onTerminate();
    }

    @Override
    public void onTimeChanged() {
        context.userService().forceRemoteLogin();
        logoutCurrentUser();
    }

    @Override
    public void onTimeZoneChanged() {
        context.userService().forceRemoteLogin();
        logoutCurrentUser();
    }

    public CampaignRepository getCampaignRepository() {
        if (campaignRepository == null) {
            campaignRepository = new CampaignRepository(getRepository());
        }
        return campaignRepository;
    }

    public TaskRepository getTaskRepository() {
        if (taskRepository == null) {
            taskRepository = new TaskRepository(getRepository(), new TaskNotesRepository(getRepository()));
        }
        return taskRepository;
    }

    public StructureRepository getStructureRepository() {
        if (structureRepository == null) {
            structureRepository = new StructureRepository(getRepository());
        }
        return structureRepository;
    }

    public LocationRepository getLocationRepository() {
        if (locationRepository == null) {
            locationRepository = new LocationRepository(getRepository());
        }
        return locationRepository;
    }

    public AllSettings getSettingsRepository() {
        return getInstance().getContext().allSettings();
    }

    public void processGlobalConfigs() {
        Setting globalSettings =  getSettingsRepository().getSetting(GLOBAL_CONFIGS);
        populateGlobalConfigs(globalSettings);
    }

    private void populateGlobalConfigs(@NonNull Setting setting) {
        if (setting == null) {
            return;
        }
        try {
            JSONArray settingsArray = new JSONArray(setting.getValue());
            for (int i = 0; i < settingsArray.length(); i++) {
                JSONObject jsonObject = settingsArray.getJSONObject(i);
                String value = jsonObject.optString(VALUE, null);
                String key = jsonObject.optString(KEY, null);
                if (value != null && key != null) {
                    globalConfigs.put(key, value);
                }
            }
        } catch (JSONException e) {
            Log.e(TAG, e.getMessage());
        }
    }

    public Map<String, String> getGlobalConfigs() {
        return globalConfigs;
    }
}
