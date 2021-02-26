package org.smartregister.reveal.util;

import android.util.Pair;

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
import org.smartregister.reveal.activity.LoginActivity;

import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
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
        when(locationRepository.getAllLocationIds()).thenReturn(Collections.singletonList("122132"));
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

    @Test
    public void testFirebasePerformanceMonitoringEnabled() {
        assertEquals(true, syncConfiguration.firebasePerformanceMonitoringEnabled());
    }

    @Test
    public void testClearDataOnNewTeamLogin() {
        assertEquals(true, syncConfiguration.clearDataOnNewTeamLogin());
    }

    @Test
    public void testGetOauthClientId() {
        assertEquals(BuildConfig.OAUTH_CLIENT_ID, syncConfiguration.getOauthClientId());
    }

    @Test
    public void testGetOauthClientSecret() {
        assertEquals(BuildConfig.OAUTH_CLIENT_SECRET, syncConfiguration.getOauthClientSecret());
    }

    @Test
    public void testGetAuthenticationActivity() {
        assertEquals(LoginActivity.class, syncConfiguration.getAuthenticationActivity());
    }

    @Test
    public void testGetGlobalSettingsQueryParams() {
        List<Pair<String, String>> getGlobalSettingsQueryParams = syncConfiguration.getGlobalSettingsQueryParams();
        assertEquals("identifier", getGlobalSettingsQueryParams.get(0).first);
        assertEquals("global_configs", getGlobalSettingsQueryParams.get(0).second);
    }

    @Test
    public void testGetTopAllowedLocationLevel() {
        assertNull(syncConfiguration.getTopAllowedLocationLevel());
    }

    @Test
    public void testGetReadTimeout() {
        assertEquals(300000, syncConfiguration.getReadTimeout());
    }

}
