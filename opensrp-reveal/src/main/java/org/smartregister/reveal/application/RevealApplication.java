package org.smartregister.reveal.application;

import android.content.Intent;
import android.util.Log;

import com.crashlytics.android.Crashlytics;
import com.evernote.android.job.JobManager;
import com.mapbox.mapboxsdk.Mapbox;
import com.vijay.jsonwizard.activities.JsonWizardFormActivity;

import org.smartregister.Context;
import org.smartregister.CoreLibrary;
import org.smartregister.commonregistry.CommonFtsObject;
import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.configurableviews.helper.JsonSpecHelper;
import org.smartregister.family.FamilyLibrary;
import org.smartregister.family.activity.FamilyWizardFormActivity;
import org.smartregister.family.domain.FamilyMetadata;
import org.smartregister.family.util.DBConstants;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.receiver.SyncStatusBroadcastReceiver;
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
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.RevealSyncConfiguration;
import org.smartregister.reveal.util.Utils;
import org.smartregister.reveal.view.FamilyProfileActivity;
import org.smartregister.sync.DrishtiSyncScheduler;
import org.smartregister.view.activity.DrishtiApplication;
import org.smartregister.view.receiver.TimeChangedBroadcastReceiver;

import io.fabric.sdk.android.Fabric;

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


    private static CommonFtsObject commonFtsObject;

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
        Fabric.with(this, new Crashlytics());
        CoreLibrary.init(context, new RevealSyncConfiguration());
        ConfigurableViewsLibrary.init(context, getRepository());
        FamilyLibrary.init(context, getRepository(), getMetadata(), BuildConfig.VERSION_CODE, BuildConfig.DATABASE_VERSION);

        LocationHelper.init(Utils.ALLOWED_LEVELS, Utils.DEFAULT_LOCATION_LEVEL);

        SyncStatusBroadcastReceiver.init(this);

        jsonSpecHelper = new JsonSpecHelper(this);

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


    private FamilyMetadata getMetadata() {
        FamilyMetadata metadata = new FamilyMetadata(FamilyWizardFormActivity.class, JsonWizardFormActivity.class, FamilyProfileActivity.class);
        metadata.updateFamilyRegister(FamilyConstants.JSON_FORM.FAMILY_REGISTER, FamilyConstants.TABLE_NAME.FAMILY, FamilyConstants.EventType.FAMILY_REGISTRATION, FamilyConstants.EventType.UPDATE_FAMILY_REGISTRATION, FamilyConstants.CONFIGURATION.FAMILY_REGISTER, FamilyConstants.RELATIONSHIP.FAMILY_HEAD, FamilyConstants.RELATIONSHIP.PRIMARY_CAREGIVER);
        metadata.updateFamilyMemberRegister(FamilyConstants.JSON_FORM.FAMILY_MEMBER_REGISTER, FamilyConstants.TABLE_NAME.FAMILY_MEMBER, FamilyConstants.EventType.FAMILY_MEMBER_REGISTRATION, FamilyConstants.EventType.UPDATE_FAMILY_MEMBER_REGISTRATION, FamilyConstants.CONFIGURATION.FAMILY_MEMBER_REGISTER, FamilyConstants.RELATIONSHIP.FAMILY);
        metadata.updateFamilyDueRegister(FamilyConstants.TABLE_NAME.FAMILY_MEMBER, 20, true);
        metadata.updateFamilyActivityRegister(FamilyConstants.TABLE_NAME.FAMILY_MEMBER, Integer.MAX_VALUE, false);
        metadata.updateFamilyOtherMemberRegister(FamilyConstants.TABLE_NAME.FAMILY_MEMBER, Integer.MAX_VALUE, false);
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
        return new String[]{FamilyConstants.TABLE_NAME.FAMILY, FamilyConstants.TABLE_NAME.FAMILY_MEMBER};
    }

    private static String[] getFtsSearchFields(String tableName) {
        if (tableName.equals(FamilyConstants.TABLE_NAME.FAMILY)) {
            return new String[]{DBConstants.KEY.BASE_ENTITY_ID, DBConstants.KEY.VILLAGE_TOWN, DBConstants.KEY.FIRST_NAME,
                    DBConstants.KEY.LAST_NAME, DBConstants.KEY.UNIQUE_ID};
        } else if (tableName.equals(FamilyConstants.TABLE_NAME.FAMILY_MEMBER)) {
            return new String[]{DBConstants.KEY.BASE_ENTITY_ID, DBConstants.KEY.FIRST_NAME, DBConstants.KEY.MIDDLE_NAME,
                    DBConstants.KEY.LAST_NAME, DBConstants.KEY.UNIQUE_ID};
        }
        return null;
    }

    private static String[] getFtsSortFields(String tableName) {
        if (tableName.equals(FamilyConstants.TABLE_NAME.FAMILY)) {
            return new String[]{DBConstants.KEY.LAST_INTERACTED_WITH, DBConstants.KEY.DATE_REMOVED};
        } else if (tableName.equals(FamilyConstants.TABLE_NAME.FAMILY_MEMBER)) {
            return new String[]{DBConstants.KEY.DOB, DBConstants.KEY.DOD, DBConstants.KEY
                    .LAST_INTERACTED_WITH, DBConstants.KEY.DATE_REMOVED};
        }
        return null;
    }
}
