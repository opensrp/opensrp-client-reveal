package org.smartregister.reveal.util;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.smartregister.SyncFilter;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.repository.LocationRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.BuildConfig;

import edu.emory.mathcs.backport.java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 5/22/19.
 */
public class RevealSyncConfigurationTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private LocationRepository locationRepository;

    @Mock
    private AllSharedPreferences allSharedPreferences;

    private RevealSyncConfiguration syncConfiguration;

    @Before
    public void setUp() {
        syncConfiguration = new RevealSyncConfiguration(locationRepository, allSharedPreferences);
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
        when(locationRepository.getAllLocationIds()).thenReturn(Arrays.asList(new String[]{"123", "122132"}));
        assertEquals("123,122132", syncConfiguration.getSyncFilterValue());
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
