package org.smartregister.reveal.util;

import android.text.TextUtils;

import org.smartregister.SyncConfiguration;
import org.smartregister.SyncFilter;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.application.RevealApplication;

import java.util.List;

/**
 * Created by samuelgithengi on 11/29/18.
 */
public class RevealSyncConfiguration extends SyncConfiguration {


    @Override
    public int getSyncMaxRetries() {
        return BuildConfig.MAX_SYNC_RETRIES;
    }

    @Override
    public SyncFilter getSyncFilterParam() {
        return SyncFilter.LOCATION;
    }

    @Override
    public String getSyncFilterValue() {
        List jurisdictions = RevealApplication.getInstance().getLocationRepository().getAllLocationIds();
        return TextUtils.join(",", jurisdictions);
    }

    @Override
    public int getUniqueIdSource() {
        return BuildConfig.OPENMRS_UNIQUE_ID_SOURCE;
    }

    @Override
    public int getUniqueIdBatchSize() {
        return BuildConfig.OPENMRS_UNIQUE_ID_BATCH_SIZE;
    }

    @Override
    public int getUniqueIdInitialBatchSize() {
        return BuildConfig.OPENMRS_UNIQUE_ID_INITIAL_BATCH_SIZE;
    }

    @Override
    public boolean disableSyncToServerIfUserIsDisabled() {
        return true;
    }

    @Override
    public SyncFilter getEncryptionParam() {
        return SyncFilter.TEAM_ID;
    }

    @Override
    public boolean updateClientDetailsTable() {
        return false;
    }

    @Override
    public boolean isSyncSettings() {
        return true;
    }

    @Override
    public int getConnectTimeout() {
        return 300000;
    }

    @Override
    public int getReadTimeout() {
        return 300000;
    }

    @Override
    public boolean isSyncUsingPost() {
        return true;
    }
}
