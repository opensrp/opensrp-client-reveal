package org.smartregister.reveal.activity;

import android.content.Context;

import com.mapbox.mapboxsdk.Mapbox;
import com.mapbox.mapboxsdk.offline.OfflineManager;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.fragment.AvailableOfflineMapsFragment;
import org.smartregister.reveal.fragment.DownloadedOfflineMapsFragment;
import org.smartregister.reveal.util.TestingUtils;
import org.smartregister.reveal.view.OfflineMapsActivity;

import java.util.ArrayList;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;
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

    private OfflineMapsActivity offlineMapsActivity;

    Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        offlineMapsActivity = Robolectric.buildActivity(OfflineMapsActivity.class).create().get();
        Whitebox.setInternalState(offlineMapsActivity, "adapter", adapter );
        Whitebox.setInternalState(offlineMapsActivity, "availableOfflineMapsFragment", availableOfflineMapsFragment );
        Whitebox.setInternalState(offlineMapsActivity, "downloadedOfflineMapsFragment", downloadedOfflineMapsFragment );
        Whitebox.setInternalState(offlineMapsActivity, "offlineManager", offlineManager);

        //doReturn(offlineManager).when(OfflineManager.getInstance(context));
        doReturn(offlineManager).when(offlineMapsActivity).initOfflineManager();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(Robolectric.buildActivity(OfflineMapsActivity.class).create().get());
    }

    @Test
    public void testOnMapDownloaded() {
        offlineMapsActivity.onMapDownloaded(TestingUtils.getOfflineMapModel());
        verify(offlineMapsActivity).getOfflineDownloadedRegions(true);
    }

}
