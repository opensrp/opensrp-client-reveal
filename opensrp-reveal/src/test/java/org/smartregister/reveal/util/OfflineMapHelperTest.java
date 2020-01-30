package org.smartregister.reveal.util;

import android.support.v4.util.Pair;

import com.mapbox.mapboxsdk.offline.OfflineRegion;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.util.List;
import java.util.Map;

import static io.ona.kujaku.downloaders.MapBoxOfflineResourcesDownloader.METADATA_JSON_FIELD_REGION_NAME;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

/**
 * Created by Richard Kareko on 1/30/20.
 */

@RunWith(PowerMockRunner.class)
@PrepareForTest({OfflineMapHelper.class})
public class OfflineMapHelperTest {

    private OfflineMapHelper OfflineMapHelper;

    @Before
    public void setUp() {
        OfflineMapHelper = new OfflineMapHelper();
    }

    @Test
    public void testGetOfflineRegionInfo() throws Exception {
        OfflineRegion offlineRegion = createMockOfflineRegion();

        OfflineRegion[] offlineRegions = {offlineRegion};

        Pair<List<String>, Map<String, OfflineRegion>> actualOfflineRegionInfo = OfflineMapHelper.getOfflineRegionInfo(offlineRegions);

        assertNotNull(actualOfflineRegionInfo);
        assertNotNull(actualOfflineRegionInfo.first);
        assertNotNull(actualOfflineRegionInfo.second);

        List<String> offlineRegionNames = actualOfflineRegionInfo.first;
        assertFalse(offlineRegionNames.isEmpty());
        assertEquals("Akros_1", offlineRegionNames.get(0));

        Map<String, OfflineRegion> modelMap = actualOfflineRegionInfo.second;
        assertNotNull(modelMap.get("Akros_1"));
    }

    private OfflineRegion createMockOfflineRegion() throws Exception {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put(METADATA_JSON_FIELD_REGION_NAME, "Akros_1");

        byte[] metadata = jsonObject.toString().getBytes("utf-8");

        final OfflineRegion offlineRegion = Mockito.mock(OfflineRegion.class);

        Mockito.when(offlineRegion.getMetadata())
                .thenReturn(metadata);

        return offlineRegion;
    }

}
