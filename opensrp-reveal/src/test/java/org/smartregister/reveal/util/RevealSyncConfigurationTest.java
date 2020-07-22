package org.smartregister.reveal.util;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.SyncFilter;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.repository.LocationRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.BuildConfig;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
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
    public void getLocationSyncFilterParam() {
        Country buildCountry = BuildConfig.BUILD_COUNTRY;
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.THAILAND);
        assertEquals(SyncFilter.LOCATION, syncConfiguration.getSyncFilterParam());
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, buildCountry);
    }

    @Test
    public void getTeamSyncFilterParam() {
        Country buildCountry = BuildConfig.BUILD_COUNTRY;
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.ZAMBIA);
        assertEquals(SyncFilter.LOCATION, syncConfiguration.getSyncFilterParam());
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, buildCountry);
    }

    @Test
    public void getLocationSyncFilterValue() {
        Country buildCountry = BuildConfig.BUILD_COUNTRY;
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.ZAMBIA);
        when(allSharedPreferences.fetchRegisteredANM()).thenReturn("123");
        when(allSharedPreferences.fetchDefaultTeamId(anyString())).thenReturn("122132");
        assertEquals("122132", syncConfiguration.getSyncFilterValue());
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, buildCountry);
    }

    @Test
    public void getTeamSyncFilterValue() {
        Country buildCountry = BuildConfig.BUILD_COUNTRY;
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.ZAMBIA);
        when(allSharedPreferences.fetchRegisteredANM()).thenReturn("123");
        when(allSharedPreferences.fetchDefaultTeamId(anyString())).thenReturn("122132");
        assertEquals("122132", syncConfiguration.getSyncFilterValue());
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, buildCountry);
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

    @Test
    public void testGetConnectionTimeOut() {
        assertEquals(300000, syncConfiguration.getConnectTimeout());
    }

    @Test
    public void testIsSyncUsingPost() {
        assertTrue(syncConfiguration.isSyncUsingPost());
    }

    @Test
    public void testGetSynchronizationTags() {
        assertNull(syncConfiguration.getSynchronizedLocationTags());
    }

    @Test
    public void testGetSettingsSyncFilterParam() {
        assertEquals(SyncFilter.TEAM_ID, syncConfiguration.getSettingsSyncFilterParam());
    }

}
