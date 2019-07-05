package org.smartregister.reveal.interactor;

import android.content.Context;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.cloudant.models.Client;
import org.smartregister.cloudant.models.Event;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.FamilyConstants.EventType;
import org.smartregister.reveal.util.FamilyConstants.TABLE_NAME;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.reveal.util.TestingUtils;
import org.smartregister.sync.ClientProcessorForJava;

import java.util.ArrayList;
import java.util.Collections;
import java.util.UUID;
import java.util.concurrent.Executors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.repository.BaseRepository.TYPE_Unsynced;

/**
 * Created by samuelgithengi on 4/25/19.
 */
public class RevealFamilyProfileInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private FamilyProfileContract.Presenter presenter;

    @Mock
    private TaskUtils taskUtils;

    @Mock
    private CommonRepository commonRepository;

    @Mock
    private EventClientRepository eventClientRepository;

    @Captor
    private ArgumentCaptor<JSONArray> jsonArrayArgumentCaptor;

    private RevealFamilyProfileInteractor interactor;

    private Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        interactor = new RevealFamilyProfileInteractor(presenter);
        Whitebox.setInternalState(interactor, "taskUtils", taskUtils);
        AppExecutors appExecutors = new AppExecutors(Executors.newSingleThreadExecutor(),
                Executors.newSingleThreadExecutor(), Executors.newSingleThreadExecutor());
        Whitebox.setInternalState(interactor, "appExecutors", appExecutors);
        Whitebox.setInternalState(interactor, "commonRepository", commonRepository);
        Whitebox.setInternalState(interactor, "eventClientRepository", eventClientRepository);
    }

    @Test
    public void testGetClientProcessorForJava() {
        ClientProcessorForJava clientProcessor = interactor.getClientProcessorForJava();
        assertNotNull(clientProcessor);
        assertTrue(clientProcessor instanceof RevealClientProcessor);
    }

    @Test
    public void testGenerateTasks() {
        String baseEntityId = UUID.randomUUID().toString();
        String structureId = UUID.randomUUID().toString();
        interactor.generateTasks(context, baseEntityId, structureId);
        verify(taskUtils, timeout(ASYNC_TIMEOUT)).generateBloodScreeningTask(context, baseEntityId, structureId);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onTasksGenerated();
    }


    @Test
    public void testUpdateFamilyMemberSurnameWithoutFamilyMembers() {
        String baseEntityId = UUID.randomUUID().toString();
        interactor.updateFamilyMemberName((Client) new Client().withBaseEntityId(baseEntityId),
                new Event(), "Victor");
        verify(commonRepository, timeout(ASYNC_TIMEOUT)).findByRelational_IDs(baseEntityId);
        verify(eventClientRepository, timeout(ASYNC_TIMEOUT)).batchInsertClients(new JSONArray());
        verify(eventClientRepository, timeout(ASYNC_TIMEOUT)).batchInsertEvents(new JSONArray(), 0);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onMembersUpdated();
    }

    @Test
    public void testUpdateFamilyMemberFirstNameUpdatesClientAndGeneratesEvents() throws JSONException {
        String baseEntityId = UUID.randomUUID().toString();
        CommonPersonObjectClient client = TestingUtils.getCommonPersonObjectClient();
        CommonPersonObject familyMember = new CommonPersonObject(client.getCaseId(), null, client.getDetails(), "ec_family_member");
        familyMember.setColumnmaps(client.getColumnmaps());
        when(commonRepository.findByRelational_IDs(baseEntityId)).thenReturn(Collections.singletonList(familyMember));
        when(eventClientRepository.getClientByBaseEntityId(familyMember.getCaseId())).thenReturn(new JSONObject());
        interactor.updateFamilyMemberName((Client) new Client().withFirstName("Otale").withBaseEntityId(baseEntityId),
                new Event(), "Charity");
        verify(commonRepository, timeout(ASYNC_TIMEOUT)).findByRelational_IDs(baseEntityId);
        verify(eventClientRepository, timeout(ASYNC_TIMEOUT)).batchInsertClients(jsonArrayArgumentCaptor.capture());
        verify(eventClientRepository, timeout(ASYNC_TIMEOUT)).batchInsertEvents(jsonArrayArgumentCaptor.capture(), eq(0l));
        verify(presenter, timeout(ASYNC_TIMEOUT)).onMembersUpdated();
        assertEquals(2, jsonArrayArgumentCaptor.getAllValues().size());

        //assert Clients is updated
        JSONArray clients = jsonArrayArgumentCaptor.getAllValues().get(0);
        JSONObject clientJSONObject = clients.getJSONObject(0);
        assertEquals(1, clients.length());
        assertEquals("Otale", clientJSONObject.getString("firstName"));
        assertEquals(TYPE_Unsynced, clientJSONObject.getString("syncStatus"));

        //assert event is generated
        JSONArray events = jsonArrayArgumentCaptor.getAllValues().get(1);
        JSONObject eventJSONObject = events.getJSONObject(0);
        assertEquals(1, events.length());
        assertEquals(TYPE_Unsynced, eventJSONObject.getString("syncStatus"));
        assertEquals(EventType.UPDATE_FAMILY_MEMBER_REGISTRATION, eventJSONObject.getString("eventType"));
        assertEquals(familyMember.getCaseId(), eventJSONObject.getString("baseEntityId"));
        assertEquals(TABLE_NAME.FAMILY_MEMBER, eventJSONObject.getString("entityType"));

    }


    @Test
    public void testUpdateFamilyMemberSurnameWithErrorCallsOnMembersUpdated() {
        String baseEntityId = UUID.randomUUID().toString();
        CommonPersonObjectClient client = TestingUtils.getCommonPersonObjectClient();
        CommonPersonObject familyMember = new CommonPersonObject(client.getCaseId(), null, client.getDetails(), "ec_family_member");
        familyMember.setColumnmaps(client.getColumnmaps());
        when(commonRepository.findByRelational_IDs(baseEntityId)).thenReturn(Collections.singletonList(familyMember));
        when(eventClientRepository.getClientByBaseEntityId(familyMember.getCaseId())).thenReturn(null);
        interactor.updateFamilyMemberName((Client) new Client().withFirstName("Otale").withBaseEntityId(baseEntityId),
                new Event(), "Otala");
        verify(commonRepository, timeout(ASYNC_TIMEOUT)).findByRelational_IDs(baseEntityId);
        verify(eventClientRepository, timeout(ASYNC_TIMEOUT)).batchInsertClients(new JSONArray());
        verify(eventClientRepository, timeout(ASYNC_TIMEOUT)).batchInsertEvents(new JSONArray(), 0l);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onMembersUpdated();
    }


}
