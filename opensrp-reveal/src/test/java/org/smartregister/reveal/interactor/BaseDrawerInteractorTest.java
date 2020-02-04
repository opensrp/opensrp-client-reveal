package org.smartregister.reveal.interactor;

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

    private BaseDrawerInteractor interactor;

    private String planId = UUID.randomUUID().toString();

    private String operationalArea = UUID.randomUUID().toString();

    @Before
    public void setUp() {
        interactor = new BaseDrawerInteractor(presenter);
        when(location.getId()).thenReturn(operationalArea);
        Cache<Location> cache = new Cache<>();
        cache.get(operationalArea, () -> location);
        Whitebox.setInternalState(Utils.class, "cache", cache);
        Whitebox.setInternalState(interactor, "planDefinitionSearchRepository", planDefinitionSearchRepository);
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
}
