package org.smartregister.reveal.interactor;

import org.smartregister.domain.Location;
import org.smartregister.domain.PlanDefinition;
import org.smartregister.repository.LocationRepository;
import org.smartregister.repository.PlanDefinitionSearchRepository;
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

    private PlanDefinitionSearchRepository planDefinitionSearchRepository;

    private LocationRepository locationRepository;

    public BaseDrawerInteractor(BaseDrawerContract.Presenter presenter) {
        this.presenter = presenter;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        planDefinitionSearchRepository = RevealApplication.getInstance().getPlanDefinitionSearchRepository();
        locationRepository = RevealApplication.getInstance().getLocationRepository();
    }

    @Override
    public void fetchPlans(String jurisdictionName) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                Location operationalArea  = locationRepository.getLocationByName(jurisdictionName);
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
}
