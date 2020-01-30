package org.smartregister.reveal.activity;

import com.mapbox.mapboxsdk.offline.OfflineManager;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.fragment.AvailableOfflineMapsFragment;
import org.smartregister.reveal.fragment.DownloadedOfflineMapsFragment;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.util.TestingUtils;
import org.smartregister.reveal.view.OfflineMapsActivity;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 1/30/20.
 */

public class OfflineMapsActivityTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private ViewPagerAdapter adapter;

    @Mock
    OfflineManager offlineManager;

    @Mock
    private AvailableOfflineMapsFragment availableOfflineMapsFragment;

    @Mock
    private DownloadedOfflineMapsFragment downloadedOfflineMapsFragment;

    @Captor
    private ArgumentCaptor<Boolean> booleanArgumentCaptor;

    @Captor
    private ArgumentCaptor<OfflineMapModel> offlineMapModelArgumentCaptor;

    private OfflineMapsActivity offlineMapsActivity;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        offlineMapsActivity = Robolectric.buildActivity(OfflineMapsActivity.class).create().get();
        Whitebox.setInternalState(offlineMapsActivity, "adapter", adapter );
        Whitebox.setInternalState(offlineMapsActivity, "availableOfflineMapsFragment", availableOfflineMapsFragment );
        Whitebox.setInternalState(offlineMapsActivity, "downloadedOfflineMapsFragment", downloadedOfflineMapsFragment );
        Whitebox.setInternalState(offlineMapsActivity, "offlineManager", offlineManager);

    }

    @Test
    public void testOnCreate() {
        assertNotNull(offlineMapsActivity);
    }

    @Test
    public void testOnMapDownloaded() {
        offlineMapsActivity = spy(offlineMapsActivity);
        offlineMapsActivity.onMapDownloaded(TestingUtils.getOfflineMapModel());
        verify(offlineMapsActivity).getOfflineDownloadedRegions(booleanArgumentCaptor.capture());
        assertTrue(booleanArgumentCaptor.getValue());
    }

    @Test
    public void testOnOfflineMapDeleted() {
        OfflineMapModel expectedOfflineMapModel = TestingUtils.getOfflineMapModel();
        offlineMapsActivity = spy(offlineMapsActivity);
        when(adapter.getItem(anyInt())).thenReturn(availableOfflineMapsFragment);
        offlineMapsActivity.onOfflineMapDeleted(expectedOfflineMapModel);
        verify(availableOfflineMapsFragment).updateOperationalAreasToDownload(offlineMapModelArgumentCaptor.capture());

        OfflineMapModel actualOfflineMapModel = offlineMapModelArgumentCaptor.getValue();
        assertNotNull(actualOfflineMapModel);
        assertEquals(expectedOfflineMapModel.getLocation().getId(), actualOfflineMapModel.getLocation().getId());
    }

}