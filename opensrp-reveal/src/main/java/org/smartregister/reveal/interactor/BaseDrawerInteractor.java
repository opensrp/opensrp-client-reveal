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

/**
 * Created by samuelgithengi on 3/21/19.
 */
public class BaseDrawerInteractor implements BaseDrawerContract.Interactor {

    private AppExecutors appExecutors;

    private BaseDrawerContract.Presenter presenter;

    private PlanDefinitionSearchRepository planDefinitionSearchRepository;

    private RevealApplication application;

    private SQLiteDatabase database;

    public BaseDrawerInteractor(BaseDrawerContract.Presenter presenter) {
        this.presenter = presenter;
        application = RevealApplication.getInstance();
        database = application.getRepository().getReadableDatabase();
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

        String syncQuery = "SELECT syncStatus FROM client WHERE syncStatus <> 'Synced' AND syncStatus <> 'task_unprocessed' \n" +
                "UNION ALL\n" +
                "SELECT syncStatus FROM event WHERE syncStatus <> 'Synced' AND syncStatus <> 'task_unprocessed' \n" +
                "UNION ALL\n" +
                "SELECT sync_Status FROM task WHERE sync_Status <> 'Synced' AND sync_Status <> 'task_unprocessed' \n" +
                "UNION ALL\n" +
                "SELECT sync_Status FROM structure WHERE sync_Status <> 'Synced' AND sync_Status <> 'task_unprocessed' \n" +
                "UNION ALL\n" +
                "SELECT syncStatus FROM form_submission WHERE syncStatus <> 'Synced' AND syncStatus <> 'task_unprocessed' ";

        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                Cursor syncCursor = null;
                try
                {
                    syncCursor  = database.rawQuery(syncQuery, null);
                    Integer count = syncCursor.getCount();

                    if(count == 0)
                    {
                        RevealApplication.getInstance().setSynced(true);
                    }
                    else
                    {
                        RevealApplication.getInstance().setSynced(false);
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
                        boolean synced = application.getSynced();
                        (presenter).updateSyncStatusDisplay(synced);
                    }
                });
            }
        };
        appExecutors.diskIO().execute(runnable);
    }
}
