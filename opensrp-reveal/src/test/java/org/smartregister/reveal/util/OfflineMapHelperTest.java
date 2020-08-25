package org.smartregister.reveal.util;

import androidx.core.util.Pair;

import com.mapbox.mapboxsdk.offline.OfflineRegion;

import org.junit.Before;
import org.junit.Test;
import org.smartregister.reveal.BaseUnitTest;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

/**
 * Created by Richard Kareko on 1/30/20.
 */


public class OfflineMapHelperTest extends BaseUnitTest {

    private OfflineMapHelper OfflineMapHelper;

    @Before
    public void setUp() {
        OfflineMapHelper = new OfflineMapHelper();
    }

    @Test
    public void testGetOfflineRegionInfo() throws Exception {
        OfflineRegion offlineRegion = TestingUtils.createMockOfflineRegion();

        OfflineRegion[] offlineRegions = {offlineRegion};

        Pair<List<String>, Map<String, OfflineRegion>> actualOfflineRegionInfo = OfflineMapHelper.getOfflineRegionInfo(offlineRegions);

        assertNotNull(actualOfflineRegionInfo);
        assertNotNull(actualOfflineRegionInfo.first);
        assertNotNull(actualOfflineRegionInfo.second);

        List<String> offlineRegionNames = actualOfflineRegionInfo.first;
        assertFalse(offlineRegionNames.isEmpty());
        assertEquals(TestingUtils.DUMMY_OPERATIONAL_AREA, offlineRegionNames.get(0));

        Map<String, OfflineRegion> modelMap = actualOfflineRegionInfo.second;
        assertNotNull(modelMap.get(TestingUtils.DUMMY_OPERATIONAL_AREA));
    }


}
