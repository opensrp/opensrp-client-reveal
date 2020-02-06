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
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.AvailableOfflineMapsContract;
import org.smartregister.reveal.interactor.AvailableOfflineMapsInteractor;
import org.smartregister.reveal.model.OfflineMapModel;

import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertEquals;
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

    @Captor
    private ArgumentCaptor<String> stringArgumentCaptor;

    @Captor
    private ArgumentCaptor<List<String>> stringListArgumentCaptor;

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
        verify(interactor).fetchAvailableOAsForMapDownLoad(stringListArgumentCaptor.capture());
        assertEquals(operationalArea, stringListArgumentCaptor.getValue().get(0));

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
        verify(view).disableCheckBox(stringArgumentCaptor.capture());
        assertEquals(operationalArea, stringArgumentCaptor.getValue());

    }

    @Test
    public void testOnDownloadComplete() {
        presenter.onDownloadComplete(operationalArea);
        verify(view).moveDownloadedOAToDownloadedList(stringArgumentCaptor.capture());
        assertEquals(operationalArea, stringArgumentCaptor.getValue());

    }

    @Test
    public void testOnDownloadStopped() {
        presenter.onDownloadStopped(operationalArea);
        verify(view).removeOperationalAreaToDownload(operationalArea);
        verify(view).enableCheckBox(stringArgumentCaptor.capture());
        assertEquals(operationalArea, stringArgumentCaptor.getValue());
    }

}
