package org.smartregister.reveal.interactor;

import org.smartregister.domain.PlanDefinition;
import org.smartregister.repository.PlanDefinitionRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.util.AppExecutors;

import java.util.Set;

/**
 * Created by samuelgithengi on 3/21/19.
 */
public class BaseDrawerInteractor implements BaseDrawerContract.Interactor {

    private AppExecutors appExecutors;

    private BaseDrawerContract.Presenter presenter;

    private PlanDefinitionRepository planDefinitionRepository;

    public BaseDrawerInteractor(BaseDrawerContract.Presenter presenter) {
        this.presenter = presenter;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        planDefinitionRepository = RevealApplication.getInstance().getPlanDefinitionRepository();
    }

    @Override
    public void fetchPlans() {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                Set<PlanDefinition> planDefinitionSet = planDefinitionRepository.findAllPlanDefinitions();
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
}
