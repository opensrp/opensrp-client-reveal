package org.smartregister.reveal.presenter;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;
import org.smartregister.reveal.TestListTaskView;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.PasswordDialogUtils;
import org.smartregister.reveal.util.PreferencesUtil;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.spy;
import static org.powermock.api.mockito.PowerMockito.whenNew;

/**
 * @author Vincent Karuri
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({ListTaskPresenter.class, ValidateUserLocationPresenter.class, PasswordDialogUtils.class, PreferencesUtil.class})
public class ListTaskPresenterTest {
    private ListTaskContract.ListTaskView listTaskViewSpy;
    private ListTaskPresenter listTaskPresenter;

    @Before
    public void setUp() throws Exception {
        mockStatic(PasswordDialogUtils.class);
        mockStatic(PreferencesUtil.class);

        ListTaskInteractor listTaskInteractorMock = mock(ListTaskInteractor.class);
        whenNew(ListTaskInteractor.class).withAnyArguments().thenReturn(listTaskInteractorMock);

        ValidateUserLocationPresenter validateUserLocationPresenterMock = mock(ValidateUserLocationPresenter.class);
        whenNew(ValidateUserLocationPresenter.class).withAnyArguments().thenReturn(validateUserLocationPresenterMock);

        ListTaskContract.ListTaskView listTaskView = new TestListTaskView();
        listTaskViewSpy = spy(listTaskView);
        listTaskPresenter = new ListTaskPresenter(listTaskViewSpy);
    }

    @Test
    public void testOnMosquitoCollectionFormSavedHidesProgressDialog() throws Exception {
        listTaskPresenter.onMosquitoCollectionFormSaved();

        verify(listTaskViewSpy).hideProgressDialog();
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForIRSEventWithNullCardDetails() throws Exception {

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        PowerMockito.doNothing().when(listTaskPresenterSpy, "startForm", any(), any(), anyString());

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", Constants.Intervention.IRS);

        listTaskPresenterSpy.onLocationValidated();

        PowerMockito.verifyPrivate(listTaskPresenterSpy, times(1)).invoke("startForm", isNull(), isNull(), eq(Constants.SPRAY_EVENT));
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForIRSEventWithCardDetails() throws Exception {

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        PowerMockito.doNothing().when(listTaskPresenterSpy, "startForm", any(), any(), anyString());

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", Constants.Intervention.IRS);

        CardDetails cardDetails = new CardDetails(null, null, null, null, null, null);

        Whitebox.setInternalState(listTaskPresenterSpy, "cardDetails", cardDetails);

        Whitebox.setInternalState(listTaskPresenterSpy, "changeSprayStatus", true);

        listTaskPresenterSpy.onLocationValidated();

        PowerMockito.verifyPrivate(listTaskPresenterSpy, times(1)).invoke("startForm", isNull(), eq(cardDetails), eq(Constants.SPRAY_EVENT));
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForMosquitoCollectionEventWithNullCardDetails() throws Exception {

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        PowerMockito.doNothing().when(listTaskPresenterSpy, "startForm", any(), any(), anyString());

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", Constants.Intervention.MOSQUITO_COLLECTION);

        listTaskPresenterSpy.onLocationValidated();

        PowerMockito.verifyPrivate(listTaskPresenterSpy, times(1)).invoke("startForm", isNull(), isNull(), eq(Constants.MOSQUITO_COLLECTION_EVENT));
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForMosquitoCollectionEventWithCardDetails() throws Exception {

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        PowerMockito.doNothing().when(listTaskPresenterSpy, "startForm", any(), any(), anyString());

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", Constants.Intervention.MOSQUITO_COLLECTION);

        CardDetails cardDetails = new CardDetails(null, null, null, null, null, null);

        Whitebox.setInternalState(listTaskPresenterSpy, "cardDetails", cardDetails);

        listTaskPresenterSpy.onLocationValidated();

        PowerMockito.verifyPrivate(listTaskPresenterSpy, times(1)).invoke("startForm", isNull(), eq(cardDetails), eq(Constants.MOSQUITO_COLLECTION_EVENT));
    }
}
