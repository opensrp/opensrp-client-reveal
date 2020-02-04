package org.smartregister.reveal.presenter;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.domain.PlanDefinition;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.interactor.BaseDrawerInteractor;
import org.smartregister.reveal.util.PreferencesUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.PlanDefinitionStatus.ACTIVE;
import static org.smartregister.reveal.util.Constants.PlanDefinitionStatus.COMPLETE;

/**
 * @author Richard Kareko
 */
public class BaseDrawerPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private BaseDrawerPresenter presenter;

    @Mock
    private BaseDrawerContract.View view;

    @Mock
    private PreferencesUtil preferencesUtil;

    @Mock
    private BaseDrawerInteractor interactor;

    @Mock
    private LocationHelper locationHelper;

    @Captor
    private ArgumentCaptor<List<String>> plansCaptor;

    @Captor
    private ArgumentCaptor<String> entireTreeString;

    @Before
    public void setUp() {
        presenter = new BaseDrawerPresenter(view, mock(BaseDrawerContract.DrawerActivity.class));
        Whitebox.setInternalState(presenter, "prefsUtil", preferencesUtil);

    }

    @Test
    public void testOnPlansFetchedReturnsActivePlans() {
        PlanDefinition planDefinition = new PlanDefinition();
        planDefinition.setStatus(ACTIVE);
        planDefinition.setIdentifier("tlv_1");
        planDefinition.setTitle("Intervention Plan");
        PlanDefinition.UseContext useContext = mock(PlanDefinition.UseContext.class);
        when(useContext.getCode()).thenReturn("focus intervention");
        when(useContext.getValueCodableConcept()).thenReturn("FI");
        List<PlanDefinition.UseContext> useContextList = new ArrayList();
        useContextList.add(useContext);
        planDefinition.setUseContext(useContextList);

        Set<PlanDefinition> planDefinitionsList = Collections.singleton(planDefinition);

        presenter.onPlansFetched(planDefinitionsList);
        verify(view).showPlanSelector(plansCaptor.capture(), entireTreeString.capture());
        assertNotNull(plansCaptor.getValue());
        assertEquals("tlv_1", plansCaptor.getValue().get(0));
        assertNotNull(entireTreeString.getValue());
        assertTrue(entireTreeString.getValue().contains("tlv_1"));
        assertTrue(entireTreeString.getValue().contains("Intervention Plan"));
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);

    }

    @Test
    public void testOnPlansFetchedDoesNotReturnPlansThatAreNotActive() {
        PlanDefinition planDefinition = new PlanDefinition();
        planDefinition.setStatus(COMPLETE);
        planDefinition.setIdentifier("tlv_1");
        planDefinition.setTitle("Intervention Plan");
        PlanDefinition.UseContext useContext = mock(PlanDefinition.UseContext.class);
        when(useContext.getCode()).thenReturn("focus intervention");
        when(useContext.getValueCodableConcept()).thenReturn("FI");
        List<PlanDefinition.UseContext> useContextList = new ArrayList();
        useContextList.add(useContext);
        planDefinition.setUseContext(useContextList);

        Set<PlanDefinition> planDefinitionsList = Collections.singleton(planDefinition);

        presenter.onPlansFetched(planDefinitionsList);
        verify(view).showPlanSelector(plansCaptor.capture(), entireTreeString.capture());
        assertNotNull(plansCaptor.getValue());
        assertTrue(plansCaptor.getValue().isEmpty());
        assertNotNull(entireTreeString.getValue());
        assertTrue(entireTreeString.getValue().isEmpty());
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);

    }

    @Test
    public void testIsPlanAndOperationalAreaSelectedReturnsTrueWHenBothSelected() {

        when(preferencesUtil.getCurrentPlanId()).thenReturn("planid");
        when(preferencesUtil.getCurrentOperationalArea()).thenReturn("OA");

        assertTrue(presenter.isPlanAndOperationalAreaSelected());

    }

    @Test
    public void testIsPlanAndOperationalAreaSelectedReturnsFalseWhenPlanNotSelected() {
        when(preferencesUtil.getCurrentPlanId()).thenReturn(null);
        when(preferencesUtil.getCurrentOperationalArea()).thenReturn("OA");

        assertFalse(presenter.isPlanAndOperationalAreaSelected());

    }

    @Test
    public void testIsPlanAndOperationalAreaSelectedReturnsFalseWhenJurisdictionNotSelected() {

        when(preferencesUtil.getCurrentPlanId()).thenReturn("planid");
        when(preferencesUtil.getCurrentOperationalArea()).thenReturn(null);

        assertFalse(presenter.isPlanAndOperationalAreaSelected());

    }

    @Test
    public void testIsPlanAndOperationalAreaSelectedReturnsFalseWhenNonSelected() {

        when(preferencesUtil.getCurrentPlanId()).thenReturn(null);
        when(preferencesUtil.getCurrentOperationalArea()).thenReturn(null);

        assertFalse(presenter.isPlanAndOperationalAreaSelected());

    }


    @Test
    public void testOnOperationalAreaSelectedValidatesPlan() {
        String planId = UUID.randomUUID().toString();
        String operationArea = UUID.randomUUID().toString();
        when(preferencesUtil.getCurrentPlanId()).thenReturn(planId);
        when(preferencesUtil.getCurrentOperationalArea()).thenReturn(operationArea);
        ArrayList<String> list = new ArrayList<>(Arrays.asList("Eastern", "Chadiza", "Chadiza RHC", operationArea));
        Whitebox.setInternalState(presenter, "locationHelper", locationHelper);
        Whitebox.setInternalState(presenter, "interactor", interactor);
        presenter.onOperationalAreaSelectorClicked(list);
        verify(interactor).validateCurrentPlan(operationArea, planId);

    }

    @Test
    public void testOnPlanValidatedFailsClearsPlan() {
        presenter.onPlanValidated(false);
        verify(preferencesUtil).setCurrentPlanId("");
        verify(preferencesUtil).setCurrentPlan("");
        verify(view).setPlan("");
        verify(view).lockNavigationDrawerForSelection();
    }

    @Test
    public void testOnPlanValidatedDoesNotClearPlan() {
        presenter.onPlanValidated(true);
        verifyZeroInteractions(preferencesUtil);
        verifyZeroInteractions(interactor);
        verifyZeroInteractions(view);

    }

}
