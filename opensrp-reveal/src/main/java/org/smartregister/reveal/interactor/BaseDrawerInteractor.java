package org.smartregister.reveal.interactor;

import org.smartregister.domain.Location;
import org.smartregister.domain.PlanDefinition;
import org.smartregister.repository.PlanDefinitionSearchRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.Utils;

import java.util.Set;

/**
 * Created by samuelgithengi on 3/21/19.
 */
public class BaseDrawerInteractor implements BaseDrawerContract.Interactor {

    private AppExecutors appExecutors;

    private BaseDrawerContract.Presenter presenter;

    private PlanDefinitionSearchRepository planDefinitionSearchRepository;


    public BaseDrawerInteractor(BaseDrawerContract.Presenter presenter) {
        this.presenter = presenter;
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
}
