package org.smartregister.reveal.sync;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;
import org.smartregister.domain.Location;
import org.smartregister.domain.db.Event;
import org.smartregister.domain.db.EventClient;
import org.smartregister.domain.jsonmapping.ClientClassification;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.spy;
import static org.smartregister.reveal.util.Constants.MOSQUITO_COLLECTION_EVENT;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;

/**
 * @author Vincent Karuri
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({RevealClientProcessor.class, Utils.class, PreferencesUtil.class})
public class RevealClientProcessorPowerMockTest {

    @Test
    public void testProcessClientShouldCallProcessSprayEventWhenIsSprayEventType() throws Exception {
        mockStatic(Utils.class);
        mockStatic(PreferencesUtil.class);

        PreferencesUtil preferencesUtil = mock(PreferencesUtil.class);
        PowerMockito.when(preferencesUtil.getCurrentOperationalArea()).thenReturn("");

        PowerMockito.when(PreferencesUtil.getInstance()).thenReturn(preferencesUtil);
        PowerMockito.when(Utils.getOperationalAreaLocation(anyString())).thenReturn(new Location());

        RevealClientProcessor clientProcessor = Whitebox.newInstance(RevealClientProcessor.class);

        List<EventClient> eventClients = new ArrayList<>();
        Event event = new Event();
        event.setEventType(SPRAY_EVENT);

        EventClient eventClient = new EventClient(event, null);
        eventClients.add(eventClient);

        RevealClientProcessor clientProcessorSpy = spy(clientProcessor);

        PowerMockito.doReturn(new ClientClassification()).when(clientProcessorSpy, "assetJsonToJava", anyString(), any());
        PowerMockito.doReturn("").when(clientProcessorSpy, "processSprayEvent", any(Event.class), any(ClientClassification.class), anyBoolean());
        clientProcessorSpy.processClient(eventClients, true);

        PowerMockito.verifyPrivate(clientProcessorSpy, times(1)).invoke("processSprayEvent", eq(event), any(ClientClassification.class), anyBoolean());
    }

    @Test
    public void testProcessClientShouldCallProcessMosquitoCollectionEventWhenIsMosquitoCollectionEventType() throws Exception {
        mockStatic(Utils.class);
        mockStatic(PreferencesUtil.class);

        PreferencesUtil preferencesUtil = mock(PreferencesUtil.class);
        PowerMockito.when(preferencesUtil.getCurrentOperationalArea()).thenReturn("");

        PowerMockito.when(PreferencesUtil.getInstance()).thenReturn(preferencesUtil);
        PowerMockito.when(Utils.getOperationalAreaLocation(anyString())).thenReturn(new Location());

        RevealClientProcessor clientProcessor = Whitebox.newInstance(RevealClientProcessor.class);

        List<EventClient> eventClients = new ArrayList<>();
        Event event = new Event();
        event.setEventType(MOSQUITO_COLLECTION_EVENT);

        EventClient eventClient = new EventClient(event, null);
        eventClients.add(eventClient);

        RevealClientProcessor clientProcessorSpy = spy(clientProcessor);

        PowerMockito.doReturn(new ClientClassification()).when(clientProcessorSpy, "assetJsonToJava", anyString(), any());
        PowerMockito.doReturn("").when(clientProcessorSpy, "processMosquitoCollectionEvent", any(Event.class), any(ClientClassification.class), anyBoolean());
        clientProcessorSpy.processClient(eventClients, true);

        PowerMockito.verifyPrivate(clientProcessorSpy, times(1)).invoke("processMosquitoCollectionEvent", eq(event), any(ClientClassification.class), anyBoolean());
    }
}
