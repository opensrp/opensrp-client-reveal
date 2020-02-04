package org.smartregister.reveal.interactor;

import android.content.Context;

import net.sqlcipher.Cursor;
import net.sqlcipher.MatrixCursor;
import net.sqlcipher.database.SQLiteDatabase;

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
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.Event;
import org.smartregister.domain.db.EventClient;
import org.smartregister.family.util.Constants;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.StructureRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.BaseContract;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.TestingUtils;
import org.smartregister.reveal.util.Utils;
import org.smartregister.util.AssetHandler;
import org.smartregister.util.Cache;
import org.smartregister.util.JsonFormUtils;

import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.DETAILS;
import static org.smartregister.reveal.util.Constants.Intervention.PAOT;
import static org.smartregister.reveal.util.Constants.JsonForm.PAOT_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_UUID;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.util.JsonFormUtils.VALUE;
import static org.smartregister.util.JsonFormUtils.VALUES;

/**
 * Created by samuelgithengi on 5/23/19.
 */
public class BaseInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private BaseContract.BasePresenter presenter;

    @Mock
    private SQLiteDatabase database;

    @Mock
    private StructureRepository structureRepository;

    @Mock
    private CommonRepository commonRepository;

    @Mock
    private RevealClientProcessor clientProcessor;

    @Mock
    private EventClientRepository eventClientRepository;

    @Captor
    private ArgumentCaptor<CommonPersonObjectClient> clientArgumentCaptor;

    @Captor
    private ArgumentCaptor<JSONObject> eventCaptor;

    @Captor
    private ArgumentCaptor<List<EventClient>> eventClientCaptor;

    private BaseInteractor interactor;

    private Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        interactor = new BaseInteractor(presenter, commonRepository);
        Whitebox.setInternalState(interactor, "structureRepository", structureRepository);
        Whitebox.setInternalState(interactor, "database", database);
        Whitebox.setInternalState(interactor, "clientProcessor", clientProcessor);
        Whitebox.setInternalState(interactor, "eventClientRepository", eventClientRepository);
    }


    @Test
    public void testFetchFamilyDetails() {
        Whitebox.setInternalState(interactor, "commonRepository", commonRepository);
        String structureId = UUID.randomUUID().toString();
        String query = "SELECT base_entity_id FROM EC_FAMILY WHERE structure_id = ? AND date_removed IS NULL";
        when(database.rawQuery(query, new String[]{structureId})).thenReturn(createFamilyCursor());
        CommonPersonObjectClient family = TestingUtils.getCommonPersonObjectClient();
        CommonPersonObject familyObject = new CommonPersonObject(family.getCaseId(),
                null, family.getDetails(), "");
        familyObject.setColumnmaps(family.getColumnmaps());
        when(commonRepository.findByBaseEntityId("69df212c-33a7-4443-a8d5-289e48d90468")).thenReturn(familyObject);
        interactor.fetchFamilyDetails(structureId);
        verify(database,timeout(ASYNC_TIMEOUT)).rawQuery(query, new String[]{structureId});
        verify(presenter, timeout(ASYNC_TIMEOUT)).onFamilyFound(clientArgumentCaptor.capture());
        assertEquals(family.entityId(), clientArgumentCaptor.getValue().entityId());
        assertEquals(family.getColumnmaps(), clientArgumentCaptor.getValue().getColumnmaps());
    }


    @Test
    public void testSavePaotForm() throws JSONException {
        Cache<Location> cache = mock(Cache.class);
        when(cache.get(anyString(), any())).thenReturn(mock(Location.class));
        Whitebox.setInternalState(Utils.class, cache);
        String form = AssetHandler.readFileFromAssetsFolder(org.smartregister.reveal.util.Constants.JsonForm.PAOT_FORM, context);
        JSONObject formObject = new JSONObject(form);
        String structureId = UUID.randomUUID().toString();
        formObject.put("entity_id", structureId);
        JSONObject details = new JSONObject();
        String taskId = UUID.randomUUID().toString();
        details.put(TASK_IDENTIFIER, taskId);
        details.put(LOCATION_UUID, structureId);
        formObject.put(DETAILS, details);
        JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(formObject), PAOT_STATUS).put(VALUE, "Active");
        JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(formObject), "lastUpdatedDate").put(VALUE, "19-07-2019");
        interactor.saveJsonForm(formObject.toString());
        verify(eventClientRepository, timeout(ASYNC_TIMEOUT)).addEvent(eq(structureId), eventCaptor.capture());
        verify(clientProcessor, timeout(ASYNC_TIMEOUT)).processClient(eventClientCaptor.capture(), eq(true));
        verify(presenter, timeout(ASYNC_TIMEOUT)).onFormSaved(structureId, taskId, Task.TaskStatus.COMPLETED, null, PAOT);
        assertEquals(org.smartregister.reveal.util.Constants.EventType.PAOT_EVENT, eventCaptor.getValue().getString("eventType"));
        JSONArray obs = eventCaptor.getValue().getJSONArray("obs");
        assertEquals(3, obs.length());
        assertEquals("Active", obs.getJSONObject(0).getJSONArray(VALUES).get(0));
        assertEquals("19-07-2019", obs.getJSONObject(1).getJSONArray(VALUES).get(0));
        assertEquals("Complete", obs.getJSONObject(2).getJSONArray(VALUES).get(0));
        assertEquals(details.toString(), eventCaptor.getValue().getJSONObject(DETAILS).toString());

        assertEquals(1, eventClientCaptor.getValue().size());

        Event event = eventClientCaptor.getValue().get(0).getEvent();
        assertEquals(3, event.getObs().size());
        assertEquals("Active", event.getObs().get(0).getValue());
        assertEquals("19-07-2019", event.getObs().get(1).getValue());
        assertEquals("Complete", event.getObs().get(2).getValue());
        assertEquals(2, event.getDetails().size());
        assertEquals(taskId, event.getDetails().get(TASK_IDENTIFIER));
        assertEquals(structureId, event.getBaseEntityId());
    }


    private Cursor createFamilyCursor() {
        MatrixCursor cursor = new MatrixCursor(new String[]{
                Constants.INTENT_KEY.BASE_ENTITY_ID
        });
        cursor.addRow(new Object[]{
                "69df212c-33a7-4443-a8d5-289e48d90468"
        });
        return cursor;
    }

}
