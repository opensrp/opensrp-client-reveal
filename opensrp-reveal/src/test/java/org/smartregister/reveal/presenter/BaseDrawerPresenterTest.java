package org.smartregister.reveal.presenter;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.smartregister.domain.PlanDefinition;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.interactor.BaseDrawerInteractor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.PreferencesUtil;

import java.util.Collections;
import java.util.Set;

import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.smartregister.reveal.util.Constants.PlanDefinitionStatus.ACTIVE;

/**
 * @author Richard Kareko
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({PreferencesUtil.class,RevealApplication.class})
public class BaseDrawerPresenterTest {

    private BaseDrawerPresenter presenter;

    @Mock
    private BaseDrawerContract.View view;

    @Mock
    BaseDrawerContract.DrawerActivity activity;

    @Mock
    BaseDrawerInteractor interactor;

    @Before
    public void setUp() throws Exception {
        mockStaticMethods();
        presenter = new BaseDrawerPresenter(view, activity);
        interactor = mock(BaseDrawerInteractor.class);
        PowerMockito.whenNew(BaseDrawerInteractor.class).withAnyArguments().thenReturn(interactor);

    }

    @Test
    public void testOnPlansFetched() {
        PlanDefinition planDefinition = new PlanDefinition();
        planDefinition.setStatus(ACTIVE);
        planDefinition.setIdentifier("tlv_1");
        planDefinition.setTitle("Intervention Plan");
        Set<PlanDefinition> planDefinitionsList = Collections.singleton(planDefinition);

        presenter.onPlansFetched(planDefinitionsList);
        verify(view).showPlanSelector(anyList(), anyString());
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
