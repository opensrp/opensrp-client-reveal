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
import org.smartregister.repository.BaseRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.presenter.StatsFragmentPresenter;
import org.smartregister.reveal.util.AppExecutors;

import java.util.Map;
import java.util.concurrent.Executors;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.SYNC_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.VALIDATION_STATUS;
import static org.smartregister.reveal.util.Constants.SyncInfo.INVALID_CLIENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.INVALID_EVENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.NULL_EVENT_SYNC_STATUS;
import static org.smartregister.reveal.util.Constants.SyncInfo.SYNCED_CLIENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.SYNCED_EVENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.TASK_UNPROCESSED_EVENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.UNSYNCED_CLIENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.UNSYNCED_EVENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.VALID_CLIENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.VALID_EVENTS;

/**
 * Created by Richard Kareko on 4/20/20.
 */

public class StatsFragmentInteractorTest extends BaseUnitTest {
    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private SQLiteDatabase database;

    @Mock
    private StatsFragmentPresenter presenter;

    @Captor
    private ArgumentCaptor<Map<String, Integer>> syncInfoMapArgumentCaptor;

    private  StatsFragmentInteractor interactor;

    private String eventSyncSql;

    private String clientSyncSql;

    private String validatedEventsSql;

    private String validatedClientsSql;

    @Before
    public void setUp() {
        AppExecutors appExecutors = new AppExecutors(Executors.newSingleThreadExecutor(),
                Executors.newSingleThreadExecutor(), Executors.newSingleThreadExecutor());
        interactor = new StatsFragmentInteractor(presenter);
        Whitebox.setInternalState(interactor, "appExecutors", appExecutors);
        Whitebox.setInternalState(interactor, "database", database);
        Whitebox.setInternalState(interactor, "presenter", presenter);

        eventSyncSql = "select count(*), syncStatus from event group by syncStatus";
        clientSyncSql = "select count(*), syncStatus from client group by syncStatus";
        validatedEventsSql = "select count(*), validationStatus from event group by validationStatus";
        validatedClientsSql = "select count(*), validationStatus from client group by validationStatus";
    }

    @Test
    public void testFetchSyncedAndValidatedECSyncInfo() {
        when(database.rawQuery(eventSyncSql, new String[]{})).thenReturn(createSyncCursor(BaseRepository.TYPE_Synced, 12));
        when(database.rawQuery(clientSyncSql, new String[]{})).thenReturn(createSyncCursor(BaseRepository.TYPE_Synced, 8));
        when(database.rawQuery(validatedEventsSql, new String[]{})).thenReturn(createValidationCursor(BaseRepository.TYPE_Valid, 11));
        when(database.rawQuery(validatedClientsSql, new String[]{})).thenReturn(createValidationCursor(BaseRepository.TYPE_Valid, 9));
        interactor.fetchECSyncInfo();

        verify(presenter, timeout(ASYNC_TIMEOUT)).onECSyncInfoFetched(syncInfoMapArgumentCaptor.capture());
        assertEquals(12, syncInfoMapArgumentCaptor.getValue().get(SYNCED_EVENTS).longValue());
        assertEquals(8, syncInfoMapArgumentCaptor.getValue().get(SYNCED_CLIENTS).longValue());
        assertEquals(11, syncInfoMapArgumentCaptor.getValue().get(VALID_EVENTS).longValue());
        assertEquals(9, syncInfoMapArgumentCaptor.getValue().get(VALID_CLIENTS).longValue());
    }

    @Test
    public void testFetchUnSyncedAndInValidatedECSyncInfo() {
        when(database.rawQuery(eventSyncSql, new String[]{})).thenReturn(createSyncCursor(BaseRepository.TYPE_Unsynced, 12));
        when(database.rawQuery(clientSyncSql, new String[]{})).thenReturn(createSyncCursor(BaseRepository.TYPE_Unsynced, 8));
        when(database.rawQuery(validatedEventsSql, new String[]{})).thenReturn(createValidationCursor(BaseRepository.TYPE_InValid, 11));
        when(database.rawQuery(validatedClientsSql, new String[]{})).thenReturn(createValidationCursor(BaseRepository.TYPE_InValid, 9));
        interactor.fetchECSyncInfo();

        verify(presenter, timeout(ASYNC_TIMEOUT)).onECSyncInfoFetched(syncInfoMapArgumentCaptor.capture());
        assertEquals(12, syncInfoMapArgumentCaptor.getValue().get(UNSYNCED_EVENTS).longValue());
        assertEquals(8, syncInfoMapArgumentCaptor.getValue().get(UNSYNCED_CLIENTS).longValue());
        assertEquals(11, syncInfoMapArgumentCaptor.getValue().get(INVALID_EVENTS).longValue());
        assertEquals(9, syncInfoMapArgumentCaptor.getValue().get(INVALID_CLIENTS).longValue());
    }

    @Test
    public void testFetchTaskUnProcessedEventsECSyncInfo() {
        when(database.rawQuery(eventSyncSql, new String[]{})).thenReturn(createSyncCursor(BaseRepository.TYPE_Task_Unprocessed, 12));
        when(database.rawQuery(clientSyncSql, new String[]{})).thenReturn(createSyncCursor(BaseRepository.TYPE_Unsynced, 6));
        when(database.rawQuery(validatedEventsSql, new String[]{})).thenReturn(createValidationCursor(BaseRepository.TYPE_InValid, 5));
        when(database.rawQuery(validatedClientsSql, new String[]{})).thenReturn(createValidationCursor(BaseRepository.TYPE_InValid, 7));
        interactor.fetchECSyncInfo();

        verify(presenter, timeout(ASYNC_TIMEOUT)).onECSyncInfoFetched(syncInfoMapArgumentCaptor.capture());
        assertEquals(12, syncInfoMapArgumentCaptor.getValue().get(TASK_UNPROCESSED_EVENTS).longValue());
        assertEquals(6, syncInfoMapArgumentCaptor.getValue().get(UNSYNCED_CLIENTS).longValue());
        assertEquals(5, syncInfoMapArgumentCaptor.getValue().get(INVALID_EVENTS).longValue());
        assertEquals(7, syncInfoMapArgumentCaptor.getValue().get(INVALID_CLIENTS).longValue());
    }

    @Test
    public void testFetchNullEventsECSyncInfo() {
        when(database.rawQuery(eventSyncSql, new String[]{})).thenReturn(createSyncCursor(null, 12));
        when(database.rawQuery(clientSyncSql, new String[]{})).thenReturn(createSyncCursor(BaseRepository.TYPE_Unsynced, 4));
        when(database.rawQuery(validatedEventsSql, new String[]{})).thenReturn(createValidationCursor(BaseRepository.TYPE_InValid, 3));
        when(database.rawQuery(validatedClientsSql, new String[]{})).thenReturn(createValidationCursor(BaseRepository.TYPE_InValid, 1));
        interactor.fetchECSyncInfo();

        verify(presenter, timeout(ASYNC_TIMEOUT)).onECSyncInfoFetched(syncInfoMapArgumentCaptor.capture());
        assertEquals(12, syncInfoMapArgumentCaptor.getValue().get(NULL_EVENT_SYNC_STATUS).longValue());
        assertEquals(4, syncInfoMapArgumentCaptor.getValue().get(UNSYNCED_CLIENTS).longValue());
        assertEquals(3, syncInfoMapArgumentCaptor.getValue().get(INVALID_EVENTS).longValue());
        assertEquals(1, syncInfoMapArgumentCaptor.getValue().get(INVALID_CLIENTS).longValue());
    }

    private Cursor createSyncCursor(String syncStatus, int count) {
        MatrixCursor cursor = new MatrixCursor(new String[]{
                "count",
                SYNC_STATUS
        });
        cursor.addRow(new Object[]{
                count,
                syncStatus,
        });
        return cursor;
    }

    private Cursor createValidationCursor(String validationStatus, int count) {
        MatrixCursor cursor = new MatrixCursor(new String[]{
                "count",
                VALIDATION_STATUS
        });
        cursor.addRow(new Object[]{
                count,
                validationStatus,
        });
        return cursor;
    }


}
