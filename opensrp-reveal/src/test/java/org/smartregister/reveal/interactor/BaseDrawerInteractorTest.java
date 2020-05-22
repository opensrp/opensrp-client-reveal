package org.smartregister.reveal.interactor;

import net.sqlcipher.Cursor;
import net.sqlcipher.MatrixCursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.domain.Location;
import org.smartregister.domain.PlanDefinition;
import org.smartregister.repository.PlanDefinitionSearchRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.util.Utils;
import org.smartregister.util.Cache;

import java.util.Collections;
import java.util.Set;
import java.util.UUID;

import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 2/4/20.
 */
public class BaseDrawerInteractorTest extends BaseUnitTest {


    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private BaseDrawerContract.Presenter presenter;

    @Mock
    private Location location;

    @Mock
    private PlanDefinition planDefinition;

    @Mock
    private PlanDefinitionSearchRepository planDefinitionSearchRepository;

    @Mock
    private SQLiteDatabase database;

    private BaseDrawerInteractor interactor;

    private String planId = UUID.randomUUID().toString();

    private String operationalArea = UUID.randomUUID().toString();
    String syncQuery = "SELECT syncStatus FROM client WHERE syncStatus <> 'Synced' AND syncStatus <> 'task_unprocessed' \n" +
            "UNION ALL\n" +
            "SELECT syncStatus FROM event WHERE syncStatus <> 'Synced' AND syncStatus <> 'task_unprocessed' \n" +
            "UNION ALL\n" +
            "SELECT sync_Status FROM task WHERE sync_Status <> 'Synced' AND sync_Status <> 'task_unprocessed' \n" +
            "UNION ALL\n" +
            "SELECT sync_Status FROM structure WHERE sync_Status <> 'Synced' AND sync_Status <> 'task_unprocessed' \n" +
            "UNION ALL\n" +
            "SELECT syncStatus FROM form_submission WHERE syncStatus <> 'Synced' AND syncStatus <> 'task_unprocessed' ";

    @Before
    public void setUp() {
        interactor = new BaseDrawerInteractor(presenter);
        when(location.getId()).thenReturn(operationalArea);
        Cache<Location> cache = new Cache<>();
        cache.get(operationalArea, () -> location);
        Whitebox.setInternalState(Utils.class, "cache", cache);
        Whitebox.setInternalState(interactor, "planDefinitionSearchRepository", planDefinitionSearchRepository);
        Whitebox.setInternalState(interactor, "database", database);
    }

    @Test
    public void testValidateCurrentPlanFailure() {
        interactor.validateCurrentPlan(planId, operationalArea);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onPlanValidated(false);

    }


    @Test
    public void testValidateCurrentPlanSuccess() {
        when(planDefinitionSearchRepository.planExists(planId, operationalArea)).thenReturn(true);
        interactor.validateCurrentPlan(operationalArea, planId);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onPlanValidated(true);
        verify(planDefinitionSearchRepository, timeout(ASYNC_TIMEOUT)).planExists(planId, operationalArea);

    }


    @Test
    public void testFetchPlans() {
        Set<PlanDefinition> expected = Collections.singleton(planDefinition);
        when(planDefinitionSearchRepository.findActivePlansByJurisdiction(operationalArea)).thenReturn(expected);
        interactor.fetchPlans(operationalArea);
        //verify(presenter, timeout(ASYNC_TIMEOUT)).onPlansFetched(expected);
        verify(planDefinitionSearchRepository, timeout(ASYNC_TIMEOUT)).findActivePlansByJurisdiction(operationalArea);

    }

    @Test
    public void testCheckSyncedTrue() {
        when(database.rawQuery(syncQuery, null)).thenReturn(emptyCursor());
        //Cursor cursorSpy = Mockito.spy(Cursor.class);
        interactor.checkSynced();
        verify(database, timeout(ASYNC_TIMEOUT)).rawQuery(syncQuery, null);
        //verify(cursorSpy).close();
        Assert.assertTrue(RevealApplication.getInstance().getSynced());
    }

    @Test
    public void testCheckSyncedFalse() {
        when(database.rawQuery(syncQuery, null)).thenReturn(populatedCursor());
        interactor.checkSynced();
        verify(database, timeout(ASYNC_TIMEOUT)).rawQuery(syncQuery, null);
        Assert.assertFalse(RevealApplication.getInstance().getSynced());
    }

    private Cursor populatedCursor() {
        MatrixCursor cursor = new MatrixCursor(new String[]{
                "syncStatus"
        });
        cursor.addRow(new Object[]{
                "Pending"
        });
        return cursor;
    }

    private Cursor emptyCursor() {
        MatrixCursor cursor = new MatrixCursor(new String[]{
                "syncStatus"
        });
        return cursor;
    }
}
