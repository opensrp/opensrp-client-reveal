package org.smartregister.reveal.fragment;

import android.content.Context;
import androidx.core.util.Pair;
import androidx.appcompat.app.AppCompatActivity;
import android.widget.CheckBox;

import com.mapbox.mapboxsdk.offline.OfflineRegion;

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
import org.robolectric.RuntimeEnvironment;
import org.smartregister.domain.Location;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.DownloadedOfflineMapAdapter;
import org.smartregister.reveal.contract.OfflineMapDownloadCallback;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.presenter.DownloadedOfflineMapsPresenter;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.anyObject;
import static org.mockito.Mockito.verify;
import static org.smartregister.reveal.model.OfflineMapModel.OfflineMapStatus.DOWNLOADED;

/**
 * Created by Richard Kareko on 1/27/20.
 */

public class DownloadedOfflineMapsFragmentTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private DownloadedOfflineMapsPresenter presenter;

    @Mock
    private DownloadedOfflineMapAdapter adapter;

    @Mock
    private OfflineMapDownloadCallback callback;

    @Captor
    private ArgumentCaptor<List<OfflineMapModel>> offlineMapModelListCaptor;

    @Captor
    private ArgumentCaptor<OfflineMapModel> offlineMapModelCaptor;

    private DownloadedOfflineMapsFragment fragment;

    private Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes= new ArrayList<>();
        fragment = new DownloadedOfflineMapsFragment();
        AppCompatActivity activity = Robolectric.buildActivity(AppCompatActivity.class).create().start().get();
        activity.setContentView(R.layout.activity_offline_maps);
        activity.getSupportFragmentManager().beginTransaction().add(fragment, "Downloaded").commit();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(Whitebox.getInternalState(fragment, "presenter"));
    }

    @Test
    public void testOnResume() {
        fragment.onResume();
        assertNotNull(Whitebox.getInternalState(fragment, "presenter"));
    }

    @Test
    public void testOnCreateView() {
        assertNotNull(Whitebox.getInternalState(fragment, "downloadedMapsRecyclerView"));
        assertNotNull(Whitebox.getInternalState(fragment, "adapter"));
        assertNotNull(Whitebox.getInternalState(fragment, "downloadedOfflineMapModelList"));
    }

    @Test
    public void testUpdateOfflineMapsTodelete() {

        List<OfflineMapModel> originalOfflineMapsTodelete = (Whitebox.getInternalState(fragment, "offlineMapsTodelete"));
        assertNotNull(originalOfflineMapsTodelete);
        assertTrue(originalOfflineMapsTodelete.isEmpty());
        CheckBox checkBox = new CheckBox(context);
        checkBox.setChecked(true);

        OfflineMapModel expectedOfflineMapModel = TestingUtils.getOfflineMapModel();
        Location expectedLocation = expectedOfflineMapModel.getLocation();
        checkBox.setTag(R.id.offline_map_checkbox, expectedOfflineMapModel);

        fragment.updateOfflineMapsTodelete(checkBox);

        List<OfflineMapModel> updatedOfflineMapsTodelete = (Whitebox.getInternalState(fragment, "offlineMapsTodelete"));
        assertNotNull(updatedOfflineMapsTodelete);
        assertFalse(updatedOfflineMapsTodelete.isEmpty());
        assertEquals(1, updatedOfflineMapsTodelete.size());
        assertEquals(expectedLocation.getId(), updatedOfflineMapsTodelete.get(0).getLocation().getId());
    }

    @Test
    public void testSetDownloadedOfflineMapModelList() {

        Whitebox.setInternalState(fragment, "adapter", adapter);

        OfflineMapModel model = TestingUtils.getOfflineMapModel();
        List<OfflineMapModel> expectedOfflineMapModels = Collections.singletonList(model);
        fragment.setDownloadedOfflineMapModelList(expectedOfflineMapModels);

        verify(adapter).setOfflineMapModels(offlineMapModelListCaptor.capture());
        assertNotNull(offlineMapModelListCaptor.getValue());

        List<OfflineMapModel> actualOfflineMapModels = offlineMapModelListCaptor.getValue();
        assertFalse(actualOfflineMapModels.isEmpty());
        assertEquals(expectedOfflineMapModels.get(0).getOfflineMapStatus(), actualOfflineMapModels.get(0).getOfflineMapStatus());
        assertEquals(expectedOfflineMapModels.get(0).getLocation().getId(), actualOfflineMapModels.get(0).getLocation().getId());

    }

    @Test
    public void testUpdateOperationalAreasToDownloadOfflineModel() {
        List<OfflineMapModel> originalDownloadedOfflineMapModelList = (Whitebox.getInternalState(fragment, "downloadedOfflineMapModelList"));
        assertTrue(originalDownloadedOfflineMapModelList.isEmpty());

        OfflineMapModel expectedOfflineMapModel = TestingUtils.getOfflineMapModel();

        fragment.updateDownloadedMapsList(expectedOfflineMapModel);
        List<OfflineMapModel> updatedOfflineMapModelList = (Whitebox.getInternalState(fragment, "downloadedOfflineMapModelList"));
        assertFalse(originalDownloadedOfflineMapModelList.isEmpty());
        assertEquals(1, updatedOfflineMapModelList.size());
        assertEquals(expectedOfflineMapModel.getLocation().getId(), updatedOfflineMapModelList.get(0).getLocation().getId());
    }

    @Test
    public void testMapDeletedSuccessfully() {
        Whitebox.setInternalState(fragment, "callback", callback);
        OfflineMapModel expectedOfflineMapModel = TestingUtils.getOfflineMapModel();
        expectedOfflineMapModel.setOfflineMapStatus(DOWNLOADED);
        List<OfflineMapModel> offlineMapModelList =  new ArrayList<>(Collections.singletonList(expectedOfflineMapModel));
        Whitebox.setInternalState(fragment, "downloadedOfflineMapModelList", offlineMapModelList);

        List<OfflineMapModel> originalDownloadedOfflineMapModelList = Whitebox.getInternalState(fragment, "downloadedOfflineMapModelList");
        assertNotNull(originalDownloadedOfflineMapModelList);
        assertEquals(1, originalDownloadedOfflineMapModelList.size());
        assertEquals(expectedOfflineMapModel.getLocation().getId(), originalDownloadedOfflineMapModelList.get(0).getLocation().getId());
        assertEquals(DOWNLOADED, originalDownloadedOfflineMapModelList.get(0).getOfflineMapStatus());

        fragment.mapDeletedSuccessfully(expectedOfflineMapModel.getDownloadAreaId());

        verify(callback).onOfflineMapDeleted(offlineMapModelCaptor.capture());
        List<OfflineMapModel> updatedDownloadedOfflineMapModelList = Whitebox.getInternalState(fragment, "downloadedOfflineMapModelList");
        assertTrue(updatedDownloadedOfflineMapModelList.isEmpty());

    }

    @Test
    public void testSetOfflineMapDownloadCallback() {
        OfflineMapDownloadCallback originalCallback = Whitebox.getInternalState(fragment, "callback");
        assertNull(originalCallback);

        fragment.setOfflineMapDownloadCallback(callback);

        OfflineMapDownloadCallback updatedCallback = Whitebox.getInternalState(fragment, "callback");
        assertNotNull(updatedCallback);
    }

    @Test
    public void testSetOfflineDownloadedMapNames() {
        Whitebox.setInternalState(fragment, "presenter", presenter);

        Map<String, OfflineRegion> offlineRegionMap = new HashMap<>();
        Pair<List<String>, Map<String, OfflineRegion>> offlineRegionInfo =
                new Pair<>(Collections.singletonList("Akros_1"), offlineRegionMap);

        fragment.setOfflineDownloadedMapNames(offlineRegionInfo);

        verify(presenter).fetchOAsWithOfflineDownloads(anyObject());
    }

}
