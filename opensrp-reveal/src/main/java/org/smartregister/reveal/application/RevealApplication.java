package org.smartregister.reveal.application;

import android.content.Intent;
import android.util.Log;

import org.smartregister.Context;
import org.smartregister.CoreLibrary;
import org.smartregister.commonregistry.CommonFtsObject;
import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.configurableviews.helper.ConfigurableViewsHelper;
import org.smartregister.configurableviews.helper.JsonSpecHelper;
import org.smartregister.configurableviews.repository.ConfigurableViewsRepository;
import org.smartregister.configurableviews.service.PullConfigurableViewsIntentService;
import org.smartregister.immunization.ImmunizationLibrary;
import org.smartregister.immunization.domain.VaccineSchedule;
import org.smartregister.immunization.domain.jsonmapping.VaccineGroup;
import org.smartregister.immunization.util.VaccinatorUtils;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.receiver.SyncStatusBroadcastReceiver;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.Repository;
import org.smartregister.reveal.activity.LoginActivity;
import org.smartregister.reveal.repository.RevealRepository;
import org.smartregister.reveal.util.DBConstants;
import org.smartregister.reveal.util.Utils;
import org.smartregister.sync.DrishtiSyncScheduler;
import org.smartregister.ug.reveal.BuildConfig;
import org.smartregister.ug.reveal.R;
import org.smartregister.view.activity.DrishtiApplication;
import org.smartregister.view.receiver.TimeChangedBroadcastReceiver;

import java.util.List;

import static org.smartregister.util.Log.logError;
import static org.smartregister.util.Log.logInfo;

/**
 * Created by ndegwamartin on 15/03/2018.
 */
public class RevealApplication extends DrishtiApplication implements TimeChangedBroadcastReceiver.OnTimeChangedListener {

    private static final String TAG = RevealApplication.class.getCanonicalName();
    private static JsonSpecHelper jsonSpecHelper;
    private static CommonFtsObject commonFtsObject;
    private ConfigurableViewsRepository configurableViewsRepository;
    private EventClientRepository eventClientRepository;
//    private UniqueIdRepository uniqueIdRepository;
    private ConfigurableViewsHelper configurableViewsHelper;
    private String password;

    public static synchronized RevealApplication getInstance() {
        return (RevealApplication) mInstance;
    }

    public static JsonSpecHelper getJsonSpecHelper() {
        return getInstance().jsonSpecHelper;
    }

    public static CommonFtsObject createCommonFtsObject() {
        if (commonFtsObject == null) {
            commonFtsObject = new CommonFtsObject(getFtsTables());
            for (String ftsTable : commonFtsObject.getTables()) {
                commonFtsObject.updateSearchFields(ftsTable, getFtsSearchFields());
                commonFtsObject.updateSortFields(ftsTable, getFtsSortFields());
            }
        }
        return commonFtsObject;
    }

    private static String[] getFtsTables() {
        return new String[]{DBConstants.PATIENT_TABLE_NAME};
    }

    private static String[] getFtsSearchFields() {
        return new String[]{DBConstants.KEY.OPENSRP_ID, DBConstants.KEY.FIRST_NAME, DBConstants.KEY.LAST_NAME, DBConstants.KEY.DATE_REMOVED, DBConstants.KEY.CARETAKER_NAME, DBConstants.KEY.CARETAKER_PHONE, DBConstants.KEY.VHT_NAME, DBConstants.KEY.VHT_PHONE};

    }

    private static String[] getFtsSortFields() {
        return new String[]{DBConstants.KEY.OPENSRP_ID, DBConstants.KEY.FIRST_NAME, DBConstants.KEY.LAST_NAME
                , DBConstants.KEY.LAST_INTERACTED_WITH, DBConstants.KEY.DATE_REMOVED};
    }

    @Override
    public void onCreate() {

        super.onCreate();

        mInstance = this;
        context = Context.getInstance();

        context.updateApplicationContext(getApplicationContext());
        context.updateCommonFtsObject(createCommonFtsObject());

        //Initialize Modules
        CoreLibrary.init(context);
        ConfigurableViewsLibrary.init(context, getRepository());
        ImmunizationLibrary.init(context, getRepository(), createCommonFtsObject(), BuildConfig.VERSION_CODE, BuildConfig.DATABASE_VERSION);

        TimeChangedBroadcastReceiver.init(this);
        TimeChangedBroadcastReceiver.getInstance().addOnTimeChangedListener(this);
        LocationHelper.init(Utils.ALLOWED_LEVELS, Utils.DEFAULT_LOCATION_LEVEL);

        startPullConfigurableViewsIntentService(getApplicationContext());
        try {
            Utils.saveLanguage("en");
        } catch (Exception e) {
            Log.e(TAG, e.getMessage());
        }

        this.jsonSpecHelper = new JsonSpecHelper(this);
        initOfflineSchedules();
    }

    @Override
    public Repository getRepository() {
        try {
            if (repository == null) {
                repository = new RevealRepository(getInstance().getApplicationContext(), context);
                getConfigurableViewsRepository();
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

    public void startPullConfigurableViewsIntentService(android.content.Context context) {
        Intent intent = new Intent(context, PullConfigurableViewsIntentService.class);
        context.startService(intent);
    }

    private void initOfflineSchedules() {
        try {
            List<VaccineGroup> childVaccines = VaccinatorUtils.getSupportedVaccines(this);
            VaccineSchedule.init(childVaccines, null, "child");
        } catch (Exception e) {
            Log.e(TAG, Log.getStackTraceString(e));
        }
    }

    public ConfigurableViewsRepository getConfigurableViewsRepository() {
        if (configurableViewsRepository == null)
            configurableViewsRepository = new ConfigurableViewsRepository(getRepository());
        return configurableViewsRepository;
    }

    public EventClientRepository getEventClientRepository() {
        if (eventClientRepository == null) {
            eventClientRepository = new EventClientRepository(getRepository());
        }
        return eventClientRepository;
    }

    public ConfigurableViewsHelper getConfigurableViewsHelper() {
        if (configurableViewsHelper == null) {
            configurableViewsHelper = new ConfigurableViewsHelper(getConfigurableViewsRepository(),
                    getJsonSpecHelper(), getApplicationContext());
        }
        return configurableViewsHelper;
    }

    @Override
    public void onTimeChanged() {
        Utils.showToast(this, this.getString(R.string.device_time_changed));
        context.userService().forceRemoteLogin();
        logoutCurrentUser();
    }


    @Override
    public void onTimeZoneChanged() {
        Utils.showToast(this, this.getString(R.string.device_timezone_changed));
        context.userService().forceRemoteLogin();
        logoutCurrentUser();
    }

}
