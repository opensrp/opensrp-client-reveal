package org.smartregister.reveal.presenter;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.smartregister.domain.PlanDefinition;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.interactor.BaseDrawerInteractor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.PreferencesUtil;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.smartregister.reveal.util.Constants.PlanDefinitionStatus.ACTIVE;
import static org.smartregister.reveal.util.Constants.PlanDefinitionStatus.COMPLETE;

/**
 * @author Richard Kareko
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({PreferencesUtil.class,RevealApplication.class})
public class BaseDrawerPresenterTest extends BaseUnitTest {

    private BaseDrawerPresenter presenter;

    @Mock
    private BaseDrawerContract.View view;

    private BaseDrawerInteractor interactor;

    @Captor
    private ArgumentCaptor<List<String>> plansCaptor;

    @Captor
    private ArgumentCaptor<String> entireTreeString;

    @Before
    public void setUp() throws Exception {
        mockStaticMethods();
        presenter = new BaseDrawerPresenter(view, mock(BaseDrawerContract.DrawerActivity.class));
        interactor = mock(BaseDrawerInteractor.class);
        PowerMockito.whenNew(BaseDrawerInteractor.class).withAnyArguments().thenReturn(interactor);

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

    private void mockStaticMethods() {
        mockStatic(PreferencesUtil.class);
        mockStatic(RevealApplication.class);

        PreferencesUtil preferencesUtil = mock(PreferencesUtil.class);
        PowerMockito.when(PreferencesUtil.getInstance()).thenReturn(preferencesUtil);

        RevealApplication application = mock(RevealApplication.class);
        when(RevealApplication.getInstance()).thenReturn(application);
        AppExecutors appExecutors = mock(AppExecutors.class);
        when(application.getAppExecutors()).thenReturn(appExecutors);

    }
}
