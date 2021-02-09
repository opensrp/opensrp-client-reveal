package org.smartregister.reveal.sync;

import android.content.Intent;

import com.evernote.android.job.JobManager;

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
import org.smartregister.receiver.SyncStatusBroadcastReceiver;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.shadow.ShadowNetworkUtils;
import org.smartregister.util.SyncUtils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.atLeastOnce;
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
    public void testOnHandleIntentShouldStartSync() throws Exception {
        ShadowNetworkUtils.setIsNetworkAvailable(true);
        ReflectionHelpers.setField(intentService, "syncUtils", syncUtils);
        when(syncUtils.verifyAuthorization()).thenReturn(true);
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
}
