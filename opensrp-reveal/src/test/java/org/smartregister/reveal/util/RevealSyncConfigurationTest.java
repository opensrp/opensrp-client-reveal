package org.smartregister.reveal.util;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.smartregister.SyncFilter;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.BuildConfig;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 5/22/19.
 */
@RunWith(MockitoJUnitRunner.class)
public class RevealSyncConfigurationTest {

    @Mock
    private AllSharedPreferences allSharedPreferences;

    private RevealSyncConfiguration syncConfiguration;

    @Before
    public void setup() {
        syncConfiguration = new RevealSyncConfiguration(allSharedPreferences);
    }

    @Test
    public void getSyncMaxRetries() {
        assertEquals(BuildConfig.MAX_SYNC_RETRIES, syncConfiguration.getSyncMaxRetries());
    }

    @Test
    public void getSyncFilterParam() {
        assertEquals(SyncFilter.TEAM_ID, syncConfiguration.getSyncFilterParam());
    }

    @Test
    public void getSyncFilterValue() {
        when(allSharedPreferences.fetchRegisteredANM()).thenReturn("user1");
        when(allSharedPreferences.fetchDefaultTeamId("user1")).thenReturn("team_123");
        assertEquals("team_123", syncConfiguration.getSyncFilterValue());
    }

    @Test
    public void getUniqueIdSource() {
        assertEquals(BuildConfig.OPENMRS_UNIQUE_ID_SOURCE, syncConfiguration.getUniqueIdSource());
    }

    @Test
    public void getUniqueIdBatchSize() {
        assertEquals(BuildConfig.OPENMRS_UNIQUE_ID_BATCH_SIZE, syncConfiguration.getUniqueIdBatchSize());
    }

    @Test
    public void getUniqueIdInitialBatchSize() {
        assertEquals(BuildConfig.OPENMRS_UNIQUE_ID_INITIAL_BATCH_SIZE, syncConfiguration.getUniqueIdInitialBatchSize());
    }

    @Test
    public void isSyncSettings() {
        assertTrue(syncConfiguration.isSyncSettings());
    }

    @Test
    public void disableSyncToServerIfUserIsDisabled() {
        assertTrue(syncConfiguration.disableSyncToServerIfUserIsDisabled());
    }

    @Test
    public void getEncryptionParam() {
        assertEquals(SyncFilter.TEAM_ID, syncConfiguration.getEncryptionParam());
    }

    @Test
    public void updateClientDetailsTable() {
        assertFalse(syncConfiguration.updateClientDetailsTable());
    }
}
