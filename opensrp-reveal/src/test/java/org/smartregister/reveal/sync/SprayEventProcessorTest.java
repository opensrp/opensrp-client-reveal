package org.smartregister.reveal.sync;


import net.sqlcipher.database.SQLiteDatabase;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.domain.Client;
import org.smartregister.domain.Event;
import org.smartregister.domain.Obs;
import org.smartregister.domain.jsonmapping.ClientClassification;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.TestingUtils;
import org.smartregister.util.AssetHandler;

import java.util.HashMap;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.smartregister.reveal.util.Constants.JsonForm.MOP_UP;

/**
 * Created by samuelgithengi on 9/18/20.
 */
public class SprayEventProcessorTest extends BaseUnitTest {

    @Mock
    private RevealClientProcessor clientProcessor;

    @Mock
    private SQLiteDatabase sqLiteDatabase;

    private SprayEventProcessor sprayEventProcessor;
    private Event sprayedEvent;
    private ClientClassification clientClassification;

    @Captor
    private ArgumentCaptor<ClientClassification> classificationArgumentCaptor;

    @Captor
    private ArgumentCaptor<Event> eventArgumentCaptor;

    @Before
    public void setup() {
        sprayEventProcessor = new SprayEventProcessor();
        sprayedEvent = TestingUtils.getSprayedEvent();
        clientClassification = AssetHandler.assetJsonToJava(new HashMap<>(), RuntimeEnvironment.application, "ec_client_classification.json", ClientClassification.class);
    }

    @Test
    public void testProcessSprayEventShouldUpdateFormSubmissionForLocalEvents() throws Exception {
        sprayEventProcessor.processSprayEvent(clientProcessor, clientClassification, sprayedEvent, true);
        verify(clientProcessor, times(2)).processEvent(eventArgumentCaptor.capture(), eq(new Client(sprayedEvent.getBaseEntityId())), classificationArgumentCaptor.capture());

        assertEquals(2, eventArgumentCaptor.getAllValues().size());
        assertEquals(2, classificationArgumentCaptor.getAllValues().size());

        ClientClassification normalClassification = classificationArgumentCaptor.getAllValues().get(0);
        Event event1 = eventArgumentCaptor.getAllValues().get(0);
        assertEquals(1, normalClassification.case_classification_rules.size());
        assertEquals("Spray: This rule checks whether a given case belongs to Sprayed Structures", normalClassification.case_classification_rules.get(0).comment);

        assertEquals(sprayedEvent.getFormSubmissionId(), event1.getFormSubmissionId());
        assertEquals(sprayedEvent.getDetails(), event1.getDetails());

        ClientClassification ecEventsClassification = classificationArgumentCaptor.getAllValues().get(1);
        Event event2 = eventArgumentCaptor.getAllValues().get(1);
        assertEquals(1, ecEventsClassification.case_classification_rules.size());
        assertEquals("ec_events: This rule maps to ec_events table used for filled forms", ecEventsClassification.case_classification_rules.get(0).comment);

        assertEquals(sprayedEvent.getFormSubmissionId(), event2.getDetails().get(Constants.DatabaseKeys.FORM_SUBMISSION_ID));
        verifyNoMoreInteractions(sqLiteDatabase);

    }

    @Test
    public void testProcessSprayEventShouldUpdateFormSubmissionForLocalEventsWithoutMopup() throws Exception {
        sprayedEvent.setDetails(null);
        Obs mopUp = sprayedEvent.findObs(null, true, MOP_UP);
        sprayedEvent.getObs().remove(mopUp);
        sprayEventProcessor.processSprayEvent(clientProcessor, clientClassification, sprayedEvent, true);
        verify(clientProcessor, times(2)).processEvent(eventArgumentCaptor.capture(), eq(new Client(sprayedEvent.getBaseEntityId())), classificationArgumentCaptor.capture());

        assertEquals(2, eventArgumentCaptor.getAllValues().size());
        assertEquals(2, classificationArgumentCaptor.getAllValues().size());

        ClientClassification normalClassification = classificationArgumentCaptor.getAllValues().get(0);
        Event event1 = eventArgumentCaptor.getAllValues().get(0);
        assertEquals(1, normalClassification.case_classification_rules.size());
        assertEquals("Spray: This rule checks whether a given case belongs to Sprayed Structures", normalClassification.case_classification_rules.get(0).comment);

        assertEquals(sprayedEvent.getFormSubmissionId(), event1.getFormSubmissionId());
        assertEquals(sprayedEvent.getDetails(), event1.getDetails());

        ClientClassification ecEventsClassification = classificationArgumentCaptor.getAllValues().get(1);
        Event event2 = eventArgumentCaptor.getAllValues().get(1);
        assertEquals(1, ecEventsClassification.case_classification_rules.size());
        assertEquals("ec_events: This rule maps to ec_events table used for filled forms", ecEventsClassification.case_classification_rules.get(0).comment);

        assertEquals(sprayedEvent.getFormSubmissionId(), event2.getDetails().get(Constants.DatabaseKeys.FORM_SUBMISSION_ID));
        verifyNoMoreInteractions(sqLiteDatabase);

    }


    @Test
    public void testProcessSprayEventShouldClearExistingEventsAndClientProcessNew() throws Exception {
        sprayEventProcessor = new SprayEventProcessor(sqLiteDatabase);
        sprayEventProcessor.processSprayEvent(clientProcessor, clientClassification, sprayedEvent, false);
        verify(clientProcessor, times(2)).processEvent(eventArgumentCaptor.capture(), eq(new Client(sprayedEvent.getBaseEntityId())), classificationArgumentCaptor.capture());

        assertEquals(2, eventArgumentCaptor.getAllValues().size());
        assertEquals(2, classificationArgumentCaptor.getAllValues().size());

        ClientClassification normalClassification = classificationArgumentCaptor.getAllValues().get(0);
        Event event1 = eventArgumentCaptor.getAllValues().get(0);
        assertEquals(1, normalClassification.case_classification_rules.size());
        assertEquals("Spray: This rule checks whether a given case belongs to Sprayed Structures", normalClassification.case_classification_rules.get(0).comment);

        assertEquals(sprayedEvent.getFormSubmissionId(), event1.getFormSubmissionId());
        assertEquals(sprayedEvent.getDetails(), event1.getDetails());

        ClientClassification ecEventsClassification = classificationArgumentCaptor.getAllValues().get(1);
        Event event2 = eventArgumentCaptor.getAllValues().get(1);
        assertEquals(1, ecEventsClassification.case_classification_rules.size());
        assertEquals("ec_events: This rule maps to ec_events table used for filled forms", ecEventsClassification.case_classification_rules.get(0).comment);

        assertEquals(sprayedEvent.getFormSubmissionId(), event2.getDetails().get(Constants.DatabaseKeys.FORM_SUBMISSION_ID));

        verify(sqLiteDatabase).delete(Constants.Tables.EC_EVENTS_TABLE, "id like ? AND event_type=?", new String[]{sprayedEvent.getBaseEntityId() + "%", sprayedEvent.getEventType()});

    }

}
