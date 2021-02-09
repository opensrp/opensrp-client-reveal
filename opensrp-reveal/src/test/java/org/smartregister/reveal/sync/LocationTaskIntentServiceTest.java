package org.smartregister.reveal.sync;

import android.content.Intent;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.robolectric.Robolectric;
import org.robolectric.annotation.Config;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.domain.FetchStatus;
import org.smartregister.receiver.SyncStatusBroadcastReceiver;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.shadow.ShadowNetworkUtils;
import org.smartregister.util.SyncUtils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

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
        intentService = Robolectric.buildIntentService(LocationTaskIntentService.class).create().get();
    }

    @Config(shadows = {ShadowNetworkUtils.class})
    @Test
    public void testOnHandleIntentShouldSendBroadCastIfNetworkIsUnAvailable() {
        ShadowNetworkUtils.setIsNetworkAvailable(false);
        ReflectionHelpers.setField(intentService, "syncUtils", syncUtils);
        Intent intent = new Intent();
        intentService = spy(intentService);
        intentService.onHandleIntent(intent);


        verify(intentService).sendBroadcast(intentArgumentCaptor.capture());
        assertEquals(SyncStatusBroadcastReceiver.ACTION_SYNC_STATUS, intentArgumentCaptor.getAllValues().get(0).getAction());
        assertEquals(FetchStatus.noConnection, intentArgumentCaptor.getAllValues().get(0).getSerializableExtra(SyncStatusBroadcastReceiver.EXTRA_FETCH_STATUS));

    }

    @Test
    public void testOnStartCommandShouldInitSyncUtils() {
        Intent intent = new Intent();
        intentService.onStartCommand(intent, 12, 2);
        assertNotNull(ReflectionHelpers.getField(intentService, "syncUtils"));
    }
}
