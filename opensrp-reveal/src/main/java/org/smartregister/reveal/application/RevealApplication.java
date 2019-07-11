package org.smartregister.reveal.application;

import android.content.Intent;
import android.support.annotation.NonNull;

import com.crashlytics.android.Crashlytics;
import com.crashlytics.android.core.CrashlyticsCore;
import com.evernote.android.job.JobManager;
import com.mapbox.mapboxsdk.Mapbox;
import com.vijay.jsonwizard.activities.JsonWizardFormActivity;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.Context;
import org.smartregister.CoreLibrary;
import org.smartregister.commonregistry.CommonFtsObject;
import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.configurableviews.helper.JsonSpecHelper;
import org.smartregister.domain.Setting;
import org.smartregister.family.FamilyLibrary;
import org.smartregister.family.activity.FamilyWizardFormActivity;
import org.smartregister.family.domain.FamilyMetadata;
import org.smartregister.family.util.DBConstants;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.receiver.SyncStatusBroadcastReceiver;
import org.smartregister.repository.AllSettings;
import org.smartregister.repository.CampaignRepository;
import org.smartregister.repository.LocationRepository;
import org.smartregister.repository.PlanDefinitionRepository;
import org.smartregister.repository.PlanDefinitionSearchRepository;
import org.smartregister.repository.Repository;
import org.smartregister.repository.StructureRepository;
import org.smartregister.repository.TaskNotesRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.activity.LoginActivity;
import org.smartregister.reveal.job.RevealJobCreator;
import org.smartregister.reveal.repository.RevealMappingHelper;
import org.smartregister.reveal.repository.RevealRepository;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.RevealSyncConfiguration;
import org.smartregister.reveal.util.Utils;
import org.smartregister.reveal.view.FamilyProfileActivity;
import org.smartregister.sync.ClientProcessorForJava;
import org.smartregister.sync.DrishtiSyncScheduler;
import org.smartregister.util.LangUtils;
import org.smartregister.view.activity.DrishtiApplication;
import org.smartregister.view.receiver.TimeChangedBroadcastReceiver;

import java.util.HashMap;
import java.util.Map;

import io.fabric.sdk.android.Fabric;
import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.CONFIGURATION.GLOBAL_CONFIGS;
import static org.smartregister.reveal.util.Constants.CONFIGURATION.KEY;
import static org.smartregister.reveal.util.Constants.CONFIGURATION.SETTINGS;
import static org.smartregister.reveal.util.Constants.CONFIGURATION.VALUE;
import static org.smartregister.reveal.util.FamilyConstants.CONFIGURATION;
import static org.smartregister.reveal.util.FamilyConstants.EventType;
import static org.smartregister.reveal.util.FamilyConstants.JSON_FORM;
import static org.smartregister.reveal.util.FamilyConstants.RELATIONSHIP;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME;
import static org.smartregister.util.Log.logError;
import static org.smartregister.util.Log.logInfo;

public class RevealApplication extends DrishtiApplication implements TimeChangedBroadcastReceiver.OnTimeChangedListener {

    private JsonSpecHelper jsonSpecHelper;
    private String password;

    private CampaignRepository campaignRepository;
    private TaskRepository taskRepository;
    private StructureRepository structureRepository;
    private LocationRepository locationRepository;
    private PlanDefinitionRepository planDefinitionRepository;
    private PlanDefinitionSearchRepository planDefinitionSearchRepository;

    private Map<String, String> globalConfigs;

    private static CommonFtsObject commonFtsObject;

    private AppExecutors appExecutors;

    private boolean refreshMapOnEventSaved;

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
        context.updateCommonFtsObject(createCommonFtsObject());
        // Initialize Modules
        Fabric.with(this, new Crashlytics.Builder().core(new CrashlyticsCore.Builder().disabled(BuildConfig.DEBUG).build()).build());
        CoreLibrary.init(context, new RevealSyncConfiguration());
        if (BuildConfig.BUILD_COUNTRY == Country.NAMIBIA) {
            CoreLibrary.getInstance().setEcClientFieldsFile(Constants.ECClientConfig.NAMIBIA_EC_CLIENT_FIELDS);
        } else if (BuildConfig.BUILD_COUNTRY == Country.BOTSWANA) {
            CoreLibrary.getInstance().setEcClientFieldsFile(Constants.ECClientConfig.BOTSWANA_EC_CLIENT_FIELDS);
        }
        ConfigurableViewsLibrary.init(context, getRepository());
        FamilyLibrary.init(context, getRepository(), getMetadata(), BuildConfig.VERSION_CODE, BuildConfig.DATABASE_VERSION);

        LocationHelper.init(Utils.ALLOWED_LEVELS, Utils.DEFAULT_LOCATION_LEVEL);

        SyncStatusBroadcastReceiver.init(this);

        jsonSpecHelper = new JsonSpecHelper(this);
        globalConfigs = new HashMap<>();

        Mapbox.getInstance(getApplicationContext(), BuildConfig.MAPBOX_SDK_ACCESS_TOKEN);

        //init Job Manager
        JobManager.create(this).addJobCreator(new RevealJobCreator());

        if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
            LangUtils.saveLanguage(getApplicationContext(), "th");
        } else {
            LangUtils.saveLanguage(getApplicationContext(), "en");
        }

    }

    @Override
    public Repository getRepository() {
        try {
            if (repository == null) {
                repository = new RevealRepository(getInstance().getApplicationContext(), context);
            }
        } catch (UnsatisfiedLinkError e) {
            Timber.e(e,"Error on getRepository: " );

        }
        return repository;
    }

    public String getPassword() {
        if (password == null) {
            String username = getContext().allSharedPreferences().fetchRegisteredANM();
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
            Timber.e(e);
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
            structureRepository.setHelper(new RevealMappingHelper());
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
        Setting globalSettings = getSettingsRepository().getSetting(GLOBAL_CONFIGS);
        populateGlobalConfigs(globalSettings);
    }

    private void populateGlobalConfigs(@NonNull Setting setting) {
        if (setting == null) {
            return;
        }
        try {
            JSONArray settingsArray = new JSONObject(setting.getValue()).getJSONArray(SETTINGS);
            for (int i = 0; i < settingsArray.length(); i++) {
                JSONObject jsonObject = settingsArray.getJSONObject(i);
                String value = jsonObject.optString(VALUE, null);
                String key = jsonObject.optString(KEY, null);
                if (value != null && key != null) {
                    globalConfigs.put(key, value);
                }
            }
        } catch (JSONException e) {
            Timber.e(e);
        }
    }

    public Map<String, String> getGlobalConfigs() {
        return globalConfigs;
    }

    public FamilyMetadata getMetadata() {
        FamilyMetadata metadata = new FamilyMetadata(FamilyWizardFormActivity.class, JsonWizardFormActivity.class, FamilyProfileActivity.class, CONFIGURATION.UNIQUE_ID_KEY, true);
        if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
            metadata.updateFamilyRegister(JSON_FORM.THAILAND_FAMILY_REGISTER, TABLE_NAME.FAMILY, EventType.FAMILY_REGISTRATION, EventType.UPDATE_FAMILY_REGISTRATION, CONFIGURATION.FAMILY_REGISTER, RELATIONSHIP.FAMILY_HEAD, RELATIONSHIP.PRIMARY_CAREGIVER);
            metadata.updateFamilyMemberRegister(JSON_FORM.THAILAND_FAMILY_MEMBER_REGISTER, TABLE_NAME.FAMILY_MEMBER, EventType.FAMILY_MEMBER_REGISTRATION, EventType.UPDATE_FAMILY_MEMBER_REGISTRATION, CONFIGURATION.FAMILY_MEMBER_REGISTER, RELATIONSHIP.FAMILY);
        } else {
            metadata.updateFamilyRegister(JSON_FORM.FAMILY_REGISTER, TABLE_NAME.FAMILY, EventType.FAMILY_REGISTRATION, EventType.UPDATE_FAMILY_REGISTRATION, CONFIGURATION.FAMILY_REGISTER, RELATIONSHIP.FAMILY_HEAD, RELATIONSHIP.PRIMARY_CAREGIVER);
            metadata.updateFamilyMemberRegister(JSON_FORM.FAMILY_MEMBER_REGISTER, TABLE_NAME.FAMILY_MEMBER, EventType.FAMILY_MEMBER_REGISTRATION, EventType.UPDATE_FAMILY_MEMBER_REGISTRATION, CONFIGURATION.FAMILY_MEMBER_REGISTER, RELATIONSHIP.FAMILY);
        }
        metadata.updateFamilyDueRegister(TABLE_NAME.FAMILY_MEMBER, 20, true);
        metadata.updateFamilyActivityRegister(TABLE_NAME.FAMILY_MEMBER, Integer.MAX_VALUE, false);
        metadata.updateFamilyOtherMemberRegister(TABLE_NAME.FAMILY_MEMBER, Integer.MAX_VALUE, false);
        return metadata;
    }

    public static CommonFtsObject createCommonFtsObject() {
        if (commonFtsObject == null) {
            commonFtsObject = new CommonFtsObject(getFtsTables());
            for (String ftsTable : commonFtsObject.getTables()) {
                commonFtsObject.updateSearchFields(ftsTable, getFtsSearchFields(ftsTable));
                commonFtsObject.updateSortFields(ftsTable, getFtsSortFields(ftsTable));
            }
        }
        return commonFtsObject;
    }

    private static String[] getFtsTables() {
        return new String[]{TABLE_NAME.FAMILY, TABLE_NAME.FAMILY_MEMBER};
    }

    private static String[] getFtsSearchFields(String tableName) {
        if (tableName.equals(TABLE_NAME.FAMILY)) {
            return new String[]{DBConstants.KEY.BASE_ENTITY_ID, DBConstants.KEY.VILLAGE_TOWN, DBConstants.KEY.FIRST_NAME,
                    DBConstants.KEY.LAST_NAME, DBConstants.KEY.UNIQUE_ID};
        } else if (tableName.equals(TABLE_NAME.FAMILY_MEMBER)) {
            return new String[]{DBConstants.KEY.BASE_ENTITY_ID, DBConstants.KEY.FIRST_NAME, DBConstants.KEY.MIDDLE_NAME,
                    DBConstants.KEY.LAST_NAME, DBConstants.KEY.UNIQUE_ID};
        }
        return null;
    }

    private static String[] getFtsSortFields(String tableName) {
        if (tableName.equals(TABLE_NAME.FAMILY)) {
            return new String[]{DBConstants.KEY.LAST_INTERACTED_WITH, DBConstants.KEY.DATE_REMOVED};
        } else if (tableName.equals(TABLE_NAME.FAMILY_MEMBER)) {
            return new String[]{DBConstants.KEY.DOB, DBConstants.KEY.DOD, DBConstants.KEY
                    .LAST_INTERACTED_WITH, DBConstants.KEY.DATE_REMOVED};
        }
        return null;
    }

    public AppExecutors getAppExecutors() {
        if (appExecutors == null) {
            appExecutors = new AppExecutors();
        }
        return appExecutors;
    }

    public PlanDefinitionRepository getPlanDefinitionRepository() {
        if (planDefinitionRepository == null) {
            planDefinitionRepository = new PlanDefinitionRepository(getRepository());
        }
        return planDefinitionRepository;
    }

    public PlanDefinitionSearchRepository getPlanDefinitionSearchRepository() {
        if (planDefinitionSearchRepository == null) {
            planDefinitionSearchRepository = new PlanDefinitionSearchRepository(getRepository());
        }
        return planDefinitionSearchRepository;
    }

    @NonNull
    @Override
    public ClientProcessorForJava getClientProcessor() {
        return RevealClientProcessor.getInstance(this);
    }

    public boolean isRefreshMapOnEventSaved() {
        return refreshMapOnEventSaved;
    }

    public void setRefreshMapOnEventSaved(boolean refreshMapOnEventSaved) {
        this.refreshMapOnEventSaved = refreshMapOnEventSaved;
    }
}
