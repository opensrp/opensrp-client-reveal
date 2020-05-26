package org.smartregister.reveal.interactor;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.domain.Location;
import org.smartregister.domain.PlanDefinition;
import org.smartregister.repository.PlanDefinitionSearchRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.Utils;

import java.util.Set;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.SYNC_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.SYNC___STATUS;
import static org.smartregister.reveal.util.Constants.Tables.CLIENT_TABLE;
import static org.smartregister.reveal.util.Constants.Tables.EVENT_TABLE;
import static org.smartregister.reveal.util.Constants.Tables.STRUCTURE_TABLE;
import static org.smartregister.reveal.util.Constants.Tables.TASK_TABLE;

/**
 * Created by samuelgithengi on 3/21/19.
 */
public class BaseDrawerInteractor implements BaseDrawerContract.Interactor {

    private AppExecutors appExecutors;

    private BaseDrawerContract.Presenter presenter;

    private PlanDefinitionSearchRepository planDefinitionSearchRepository;

    private RevealApplication revealApplication;

    private SQLiteDatabase database;

    private String SYNCED = "Synced";

    private String TASK_UNPROCESSED = "task_unprocessed";

    public BaseDrawerInteractor(BaseDrawerContract.Presenter presenter) {
        this.presenter = presenter;
        revealApplication = RevealApplication.getInstance();
        database = revealApplication.getRepository().getReadableDatabase();
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        planDefinitionSearchRepository = RevealApplication.getInstance().getPlanDefinitionSearchRepository();
    }

    @Override
    public void fetchPlans(String jurisdictionName) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                Location operationalArea = Utils.getOperationalAreaLocation(jurisdictionName);
                String jurisdictionIdentifier = operationalArea != null ? operationalArea.getId() : null;
                Set<PlanDefinition> planDefinitionSet = planDefinitionSearchRepository.findActivePlansByJurisdiction(jurisdictionIdentifier);
                appExecutors.mainThread().execute(new Runnable() {
                    @Override
                    public void run() {
                        presenter.onPlansFetched(planDefinitionSet);
                    }
                });

            }
        };

        appExecutors.diskIO().execute(runnable);
    }

    @Override
    public void validateCurrentPlan(String selectedOperationalArea, String currentPlanId) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                Location operationalArea = Utils.getOperationalAreaLocation(selectedOperationalArea);
                String jurisdictionIdentifier = operationalArea != null ? operationalArea.getId() : null;
                boolean isValid = planDefinitionSearchRepository.planExists(currentPlanId, jurisdictionIdentifier);
                appExecutors.mainThread().execute(() -> presenter.onPlanValidated(isValid));

            }
        };

        appExecutors.diskIO().execute(runnable);

    }

    @Override
    public void checkSynced() {

        String syncQuery = String.format("SELECT %s FROM %s WHERE %s <> ?\n", SYNC_STATUS, CLIENT_TABLE, SYNC_STATUS) +
                "UNION ALL\n" +
                String.format("SELECT %s FROM %s WHERE %s <> ? AND %s <> ?\n", SYNC_STATUS, EVENT_TABLE, SYNC_STATUS, SYNC_STATUS) +
                "UNION ALL\n" +
                String.format("SELECT %s FROM %s WHERE %s <> ?\n", SYNC___STATUS, TASK_TABLE, SYNC___STATUS) +
                "UNION ALL\n" +
                String.format("SELECT %s FROM %s WHERE %s <> ?\n", SYNC___STATUS, STRUCTURE_TABLE, SYNC___STATUS);

        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                Cursor syncCursor = null;
                try
                {
                    syncCursor  = database.rawQuery(syncQuery, new String[]{SYNCED, SYNCED, TASK_UNPROCESSED, SYNCED, SYNCED});
                    Integer count = syncCursor.getCount();

                    if(count == 0)
                    {
                        revealApplication.setSynced(true);
                    }
                    else
                    {
                        revealApplication.setSynced(false);
                    }
                }
                catch (Exception e)
                {
                    Timber.e(e, "EXCEPTION %s", e.toString());
                }
                finally
                {
                    if (syncCursor != null)
                        syncCursor.close();
                }

                appExecutors.mainThread().execute(new Runnable() {
                    @Override
                    public void run() {
                        boolean synced = revealApplication.getSynced();
                        (presenter).updateSyncStatusDisplay(synced);
                    }
                });
            }
        };
        appExecutors.diskIO().execute(runnable);
    }
}
