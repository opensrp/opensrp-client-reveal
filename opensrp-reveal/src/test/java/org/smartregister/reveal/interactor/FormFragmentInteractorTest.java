package org.smartregister.reveal.interactor;


import androidx.core.util.Pair;

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
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.BaseFormFragmentContract;
import org.smartregister.reveal.util.InteractorUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static com.vijay.jsonwizard.constants.JsonFormConstants.KEY;
import static com.vijay.jsonwizard.constants.JsonFormConstants.TEXT;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.BASE_ENTITY_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FIRST_NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LAST_NAME;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;

/**
 * Created by samuelgithengi on 6/18/19.
 */
public class FormFragmentInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private BaseFormFragmentContract.Presenter presenter;

    @Mock
    private CommonRepository commonRepository;

    @Mock
    private SQLiteDatabase sqLiteDatabase;

    @Mock
    InteractorUtils interactorUtils;

    @Captor
    private ArgumentCaptor<JSONArray> jsonArgumentCaptor;

    @Captor
    private ArgumentCaptor<CommonPersonObject> commonPersonObjectCaptor;

    private BaseFormFragmentInteractor interactor;

    private static final String CASEID = "caseId";
    private static final String RELATIONID = "relationalid";

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        interactor = new BaseFormFragmentInteractor(presenter);
        Whitebox.setInternalState(interactor, "commonRepository", commonRepository);
        Whitebox.setInternalState(interactor, "sqLiteDatabase", sqLiteDatabase);
        Whitebox.setInternalState(interactor, "interactorUtils", interactorUtils);
    }

    @Test
    public void testFindNumberOfMembers() {

        String structureId = UUID.randomUUID().toString();
        String query = "SELECT count(*),SUM(CASE WHEN sleeps_outdoors='Yes' THEN 1 ELSE 0 END) FROM ec_family_member WHERE structure_id = ?";
        when(sqLiteDatabase.rawQuery(query, new String[]{structureId})).thenReturn(createNumberOfMembersCursor());
        JSONObject form = new JSONObject();
        interactor.findNumberOfMembers(structureId, form);
        verify(sqLiteDatabase, timeout(ASYNC_TIMEOUT)).rawQuery(query, new String[]{structureId});
        verify(presenter, timeout(ASYNC_TIMEOUT)).onFetchedMembersCount(new Pair<>(12, 4), form);
    }

    @Test
    public void testFindMemberDetails() throws JSONException {
        JSONObject form = new JSONObject();
        String structureId = UUID.randomUUID().toString();
        when(sqLiteDatabase.rawQuery(anyString(), any())).thenReturn(createCursor());
        interactor.findMemberDetails(structureId, form);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onFetchedFamilyMembers(jsonArgumentCaptor.capture(), eq(form));
        assertEquals(1, jsonArgumentCaptor.getValue().length());
        JSONObject member = jsonArgumentCaptor.getValue().getJSONObject(0);
        assertEquals("69df212c-33a7-4443-a8d5-289e48d90468", member.getString(KEY));
        assertEquals("Rey Mister", member.getString(TEXT));

    }

    @Test
    public void findSprayDetails() {
        JSONObject form = new JSONObject();
        String structureId = UUID.randomUUID().toString();
        CommonPersonObject commonPersonObject = createCommonPersonObject();
        when(interactorUtils.fetchSprayDetails(anyString(), anyString(), any(), any())).thenReturn(commonPersonObject);
        interactor.findSprayDetails(IRS, structureId, form);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onFetchedSprayDetails(commonPersonObjectCaptor.capture(), eq(form));
        assertNotNull(commonPersonObjectCaptor.getValue());
        assertEquals("case-id", commonPersonObjectCaptor.getValue().getCaseId());
        assertEquals("relation-id", commonPersonObjectCaptor.getValue().getRelationalId());
        assertNotNull(commonPersonObjectCaptor.getValue().getDetails());
        assertEquals("4", commonPersonObjectCaptor.getValue().getDetails().get("nSprayedDeltaMop"));
        assertEquals("3", commonPersonObjectCaptor.getValue().getDetails().get("nPeopleProtected"));

    }

    private Cursor createCursor() {
        MatrixCursor cursor = new MatrixCursor(new String[]{
                BASE_ENTITY_ID,
                FIRST_NAME,
                LAST_NAME
        });
        cursor.addRow(new Object[]{
                "69df212c-33a7-4443-a8d5-289e48d90468",
                "Rey",
                "Mister"
        });
        return cursor;
    }

    private CommonPersonObject createCommonPersonObject() {
        Map<String, String> details = new HashMap<>();
        details.put("nSprayedDeltaMop", "4");
        details.put("nPeopleProtected", "3");

        CommonPersonObject commonPersonObject = new CommonPersonObject("case-id",
                "relation-id", details, "IRS");
        return commonPersonObject;
    }


    private MatrixCursor createNumberOfMembersCursor() {
        MatrixCursor cursor = new MatrixCursor(new String[2]);
        cursor.addRow(new Object[]{
                12,
                4
        });
        return cursor;
    }
}
