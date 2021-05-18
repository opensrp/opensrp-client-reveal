package org.smartregister.reveal.sync;

import android.content.Intent;

import com.evernote.android.job.JobManager;
import com.google.firebase.FirebaseApp;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.annotation.Config;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.domain.FetchStatus;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.receiver.SyncStatusBroadcastReceiver;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.shadow.ShadowNetworkUtils;
import org.smartregister.reveal.util.TestingUtils;
import org.smartregister.reveal.util.Utils;
import org.smartregister.util.Cache;
import org.smartregister.util.SyncUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 2/9/21.
 */
public class LocationTaskIntentServiceTest extends BaseUnitTest {

    @Mock
    private SyncUtils syncUtils;

    @Captor
    private ArgumentCaptor<Intent> intentArgumentCaptor;

    private LocationTaskIntentService intentService;

    @Before
    public void setUp() {
        JobManager.create(RuntimeEnvironment.application);
        intentService = Robolectric.buildIntentService(LocationTaskIntentService.class).create().get();
    }


    @After
    public void tearDown() {
        ReflectionHelpers.setField(JobManager.instance(), "instance", null);
    }

    @Config(shadows = {ShadowNetworkUtils.class})
    @Test
    public void testOnHandleIntentShouldSendBroadCastIfNetworkIsUnAvailable() {
        ShadowNetworkUtils.setIsNetworkAvailable(false);
        Intent intent = new Intent();
        intentService = spy(intentService);
        intentService.onHandleIntent(intent);

        verify(intentService).sendBroadcast(intentArgumentCaptor.capture());
        assertEquals(SyncStatusBroadcastReceiver.ACTION_SYNC_STATUS, intentArgumentCaptor.getAllValues().get(0).getAction());
        assertEquals(FetchStatus.noConnection, intentArgumentCaptor.getAllValues().get(0).getSerializableExtra(SyncStatusBroadcastReceiver.EXTRA_FETCH_STATUS));

    }

    @Config(shadows = {ShadowNetworkUtils.class})
    @Test
    public void testOnHandleIntentShouldLogoutIfUserIsUnauthorized() throws Exception {
        ShadowNetworkUtils.setIsNetworkAvailable(true);
        ReflectionHelpers.setField(intentService, "syncUtils", syncUtils);
        when(syncUtils.verifyAuthorization()).thenReturn(false);
        Intent intent = new Intent();
        intentService = spy(intentService);
        intentService.onHandleIntent(intent);


        verify(intentService, never()).sendBroadcast(intentArgumentCaptor.capture());
        verify(syncUtils).logoutUser();
        verify(intentService).onHandleIntent(intent);
        verifyNoMoreInteractions(intentService);

    }


    @Config(shadows = {ShadowNetworkUtils.class})
    @Test
    public void testOnHandleIntentShouldStartSync() {
        ShadowNetworkUtils.setIsNetworkAvailable(true);
        ReflectionHelpers.setField(intentService, "syncUtils", syncUtils);
        when(syncUtils.verifyAuthorization()).thenReturn(true);
        FirebaseApp.initializeApp(RuntimeEnvironment.application);
        Intent intent = new Intent();
        intentService = spy(intentService);
        intentService.onHandleIntent(intent);

        verify(intentService, atLeastOnce()).sendBroadcast(intentArgumentCaptor.capture());
        for (Intent in : intentArgumentCaptor.getAllValues()) {
            assertEquals(SyncStatusBroadcastReceiver.ACTION_SYNC_STATUS, in.getAction());
            assertEquals(FetchStatus.fetchStarted, in.getSerializableExtra(SyncStatusBroadcastReceiver.EXTRA_FETCH_STATUS));
        }
        verify(intentService).doSync();

    }

    @Test
    public void testOnStartCommandShouldInitSyncUtils() {
        Intent intent = new Intent();
        intentService.onStartCommand(intent, 12, 2);
        assertNotNull(ReflectionHelpers.getField(intentService, "syncUtils"));
    }


    @Test
    public void testHasChangesInCurrentOperationalAreaWithStructuresInOA() {
        Location location = TestingUtils.gson.fromJson(TestingUtils.structureJSON, Location.class);
        List<Task> tasks = new ArrayList<>();
        List<Location> locations = new ArrayList();
        ReflectionHelpers.ClassParameter<?>[] parameters = ReflectionHelpers.ClassParameter.fromComponentLists(new Class[]{List.class, List.class}, new Object[]{locations, tasks});
        assertFalse(ReflectionHelpers.callInstanceMethod(intentService, "hasChangesInCurrentOperationalArea", parameters));

        Location operationalArea = TestingUtils.gson.fromJson(TestingUtils.operationalAreaGeoJSON, Location.class);
        Cache<Location> cache = mock(Cache.class);
        when(cache.get(anyString(), any())).thenReturn(operationalArea);
        ReflectionHelpers.setStaticField(Utils.class, "cache", cache);
        tasks = new ArrayList<>(TestingUtils.createTasks());
        locations = Collections.singletonList(location);
        parameters = ReflectionHelpers.ClassParameter.fromComponentLists(new Class[]{List.class, List.class}, new Object[]{locations, tasks});
        assertTrue(ReflectionHelpers.callInstanceMethod(intentService, "hasChangesInCurrentOperationalArea", parameters));

    }

    @Test
    public void testHasChangesInCurrentOperationalAreaWithTasksInCurrentOA() {
        List<Task> tasks = new ArrayList<>(TestingUtils.createTasks());
        List<Location> locations = Collections.emptyList();
        ReflectionHelpers.ClassParameter<?>[] parameters = ReflectionHelpers.ClassParameter.fromComponentLists(new Class[]{List.class, List.class}, new Object[]{locations, tasks});
        assertFalse(ReflectionHelpers.callInstanceMethod(intentService, "hasChangesInCurrentOperationalArea", parameters));

        Location operationalArea = TestingUtils.gson.fromJson(TestingUtils.operationalAreaGeoJSON, Location.class);
        Cache<Location> cache = mock(Cache.class);
        when(cache.get(anyString(), any())).thenReturn(operationalArea);
        ReflectionHelpers.setStaticField(Utils.class, "cache", cache);

        tasks.get(4).setGroupIdentifier(operationalArea.getId());

        assertTrue(ReflectionHelpers.callInstanceMethod(intentService, "hasChangesInCurrentOperationalArea", parameters));

    }


    @Test
    public void testHasChangesInCurrentOperationalAreaWithStructuresAndTasksDifferentOA() {
        Location location = TestingUtils.gson.fromJson(TestingUtils.structureJSON, Location.class);
        location.getProperties().setParentId(UUID.randomUUID().toString());
        List<Task> tasks = new ArrayList<>(TestingUtils.createTasks());
        List<Location> locations = Collections.singletonList(location);
        ReflectionHelpers.ClassParameter<?>[] parameters = ReflectionHelpers.ClassParameter.fromComponentLists(new Class[]{List.class, List.class}, new Object[]{locations, tasks});

        Location operationalArea = TestingUtils.gson.fromJson(TestingUtils.operationalAreaGeoJSON, Location.class);
        Cache<Location> cache = mock(Cache.class);
        when(cache.get(anyString(), any())).thenReturn(operationalArea);
        ReflectionHelpers.setStaticField(Utils.class, "cache", cache);

        assertFalse(ReflectionHelpers.callInstanceMethod(intentService, "hasChangesInCurrentOperationalArea", parameters));

    }
}
