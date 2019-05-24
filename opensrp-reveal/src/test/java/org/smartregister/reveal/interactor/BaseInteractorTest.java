package org.smartregister.reveal.interactor;

import net.sqlcipher.Cursor;
import net.sqlcipher.MatrixCursor;
import net.sqlcipher.database.SQLiteDatabase;

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
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.family.util.Constants;
import org.smartregister.repository.StructureRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.BaseContract;
import org.smartregister.reveal.util.TestingUtils;

import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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

    @Captor
    private ArgumentCaptor<CommonPersonObjectClient> clientArgumentCaptor;

    private BaseInteractor interactor;

    @Before
    public void setUp() {
        interactor = new BaseInteractor(presenter, commonRepository);
        Whitebox.setInternalState(interactor, "structureRepository", structureRepository);
        Whitebox.setInternalState(interactor, "database", database);
    }


    @Test
    public void testFetchFamilyDetails() {
        Whitebox.setInternalState(interactor, "commonRepository", commonRepository);
        String stuctureId = UUID.randomUUID().toString();
        when(database.rawQuery("SELECT base_entity_id FROM EC_FAMILY WHERE structure_id = ?", new String[]{stuctureId})).thenReturn(createFamilyCursor());
        CommonPersonObjectClient family = TestingUtils.getCommonPersonObjectClient();
        CommonPersonObject familyObject = new CommonPersonObject(family.getCaseId(),
                null, family.getDetails(), "");
        familyObject.setColumnmaps(family.getColumnmaps());
        when(commonRepository.findByBaseEntityId("69df212c-33a7-4443-a8d5-289e48d90468")).thenReturn(familyObject);
        interactor.fetchFamilyDetails(stuctureId);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onFamilyFound(clientArgumentCaptor.capture());
        assertEquals(family.entityId(), clientArgumentCaptor.getValue().entityId());
        assertEquals(family.getColumnmaps(), clientArgumentCaptor.getValue().getColumnmaps());
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
