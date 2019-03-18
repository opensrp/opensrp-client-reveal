package org.smartregister.reveal.presenter;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.smartregister.reveal.TestListTaskView;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.reveal.util.PasswordDialogUtils;
import org.smartregister.reveal.util.PreferencesUtil;

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
    private ListTaskContract.ListTaskView listTaskView = new TestListTaskView();
    private ListTaskPresenter listTaskPresenter;

    @Test
    public void testOnMosquitoCollectionFormSavedHidesProgressDialog() throws Exception { ;
        mockStatic(PasswordDialogUtils.class);
        mockStatic(PreferencesUtil.class);

        ListTaskInteractor listTaskInteractorMock = mock(ListTaskInteractor.class);
        whenNew(ListTaskInteractor.class).withAnyArguments().thenReturn(listTaskInteractorMock);

        ValidateUserLocationPresenter validateUserLocationPresenterMock = mock(ValidateUserLocationPresenter.class);
        whenNew(ValidateUserLocationPresenter.class).withAnyArguments().thenReturn(validateUserLocationPresenterMock);

        ListTaskContract.ListTaskView listTaskViewSpy = spy(listTaskView);
        listTaskPresenter = new ListTaskPresenter(listTaskViewSpy);
        listTaskPresenter.onMosquitoCollectionFormSaved();
        verify(listTaskViewSpy).hideProgressDialog();
    }
}
