package org.smartregister.reveal.presenter;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.AvailableOfflineMapsContract;
import org.smartregister.reveal.interactor.AvailableOfflineMapsInteractor;
import org.smartregister.reveal.model.OfflineMapModel;

import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.verify;

/**
 * Created by Richard Kareko on 1/24/20.
 */
public class AvailableOfflineMapsPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private AvailableOfflineMapsContract.View view;

    @Mock
    private AvailableOfflineMapsInteractor interactor;

    private AvailableOfflineMapsPresenter presenter;

    private String operationalArea = "Akros_1";

    @Before
    public void setUp() {
        presenter = new AvailableOfflineMapsPresenter(view);
        Whitebox.setInternalState(presenter, "interactor", interactor);
        operationalArea = "Akros_1";
    }

    @Test
    public void testFetchAvailableOAsForMapDownLoad() {

        presenter.fetchAvailableOAsForMapDownLoad(Collections.singletonList(operationalArea));
        verify(interactor).fetchAvailableOAsForMapDownLoad(Collections.singletonList(operationalArea));

    }

    @Test
    public void testOnFetchAvailableOAsForMapDownLoad() {

        List<OfflineMapModel> offlineMapModels = Collections.singletonList(new OfflineMapModel());
        presenter.onFetchAvailableOAsForMapDownLoad(offlineMapModels);
        verify(view).setOfflineMapModelList(offlineMapModels);

    }

    @Test
    public void testOnDownloadStarted() {

        presenter.onDownloadStarted(operationalArea);
        verify(view).disableCheckBox(operationalArea);

    }

    @Test
    public void testOnDownloadComplete() {

        presenter.onDownloadComplete(operationalArea);
        verify(view).moveDownloadedOAToDownloadedList(operationalArea);

    }

}
