package org.smartregister.reveal.activity;

import androidx.core.util.Pair;

import com.mapbox.mapboxsdk.offline.OfflineManager;
import com.mapbox.mapboxsdk.offline.OfflineRegion;

import org.json.JSONObject;
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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.ona.kujaku.data.realm.objects.MapBoxOfflineQueueTask;

import static io.ona.kujaku.downloaders.MapBoxOfflineResourcesDownloader.METADATA_JSON_FIELD_REGION_NAME;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
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
    private OfflineManager offlineManager;

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

    @Test
    public void testSetOfflineDownloadedMapNames() throws  Exception {
        offlineMapsActivity = spy(offlineMapsActivity);

        when(adapter.getItem(OfflineMapsActivity.AVAILABLE_OFFLINE_MAPS_FRAGMENT_INDEX)).thenReturn(availableOfflineMapsFragment);
        when(adapter.getItem(OfflineMapsActivity.DOWNLOADED_OFFLINE_MAPS_FRAGMENT_INDEX)).thenReturn(downloadedOfflineMapsFragment);

        Pair<List<String>, Map<String, OfflineRegion>> offlineRegionInfo = initOfflineRegionInfo();

        offlineMapsActivity.setOfflineDownloadedMapNames(offlineRegionInfo, false);
        verify(downloadedOfflineMapsFragment).setOfflineDownloadedMapNames(offlineRegionInfo);
        verify(availableOfflineMapsFragment).setOfflineDownloadedMapNames(offlineRegionInfo.first);
    }

    @Test
    public void testSetOfflineDownloadedMapNamesRefreshDownloadListOnly() throws  Exception {
        offlineMapsActivity = spy(offlineMapsActivity);

        when(adapter.getItem(OfflineMapsActivity.AVAILABLE_OFFLINE_MAPS_FRAGMENT_INDEX)).thenReturn(availableOfflineMapsFragment);
        when(adapter.getItem(OfflineMapsActivity.DOWNLOADED_OFFLINE_MAPS_FRAGMENT_INDEX)).thenReturn(downloadedOfflineMapsFragment);

        Pair<List<String>, Map<String, OfflineRegion>> offlineRegionInfo = initOfflineRegionInfo();

        offlineMapsActivity.setOfflineDownloadedMapNames(offlineRegionInfo, true);
        verify(downloadedOfflineMapsFragment).setOfflineDownloadedMapNames(offlineRegionInfo);
        verify(availableOfflineMapsFragment, times(0)).setOfflineDownloadedMapNames(offlineRegionInfo.first);
    }


    private Pair<List<String>, Map<String, OfflineRegion>> initOfflineRegionInfo() throws Exception {
        List<String> offlineRegionNames = Collections.singletonList("Akros_1");
        OfflineRegion offlineRegion = TestingUtils.createMockOfflineRegion();
        OfflineRegion[] offlineRegions = {offlineRegion};

        JSONObject task = new JSONObject();
        task.put(METADATA_JSON_FIELD_REGION_NAME, "Akros_1");

        MapBoxOfflineQueueTask offlineQueueTask = new MapBoxOfflineQueueTask();
        offlineQueueTask.setTaskStatus(MapBoxOfflineQueueTask.TASK_STATUS_DONE);
        offlineQueueTask.setTaskType(MapBoxOfflineQueueTask.TASK_TYPE_DOWNLOAD);
        offlineQueueTask.setTask(task);

        Map<String, MapBoxOfflineQueueTask> offlineQueueTaskMap = new HashMap<>();
        offlineQueueTaskMap.put("Akros", offlineQueueTask);

        return new Pair(offlineRegionNames, offlineRegions);
    }

}