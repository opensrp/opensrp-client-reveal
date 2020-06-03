package org.smartregister.reveal.shadow;

import org.mockito.Mockito;
import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;
import org.smartregister.receiver.SyncStatusBroadcastReceiver;

import java.util.ArrayList;
import java.util.List;

@Implements(SyncStatusBroadcastReceiver.class)
public class SyncStatusBroadcastReceiverShadow {

    private static SyncStatusBroadcastReceiver receiver = Mockito.mock(SyncStatusBroadcastReceiver.class);
    private static List<SyncStatusBroadcastReceiver.SyncStatusListener> listeners = new ArrayList<>();

    @Implementation
    public static SyncStatusBroadcastReceiver getInstance() {
        return receiver;
    }
}
