package org.smartregister.reveal.util;

import android.support.annotation.NonNull;
import android.support.v4.util.Pair;

import com.mapbox.mapboxsdk.offline.OfflineRegion;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.ona.kujaku.downloaders.MapBoxOfflineResourcesDownloader;
import io.ona.kujaku.utils.LogUtil;

/**
 * Created by Richard Kareko on 1/30/20.
 */

public class OfflineMapHelper {

    private static final String TAG = OfflineMapHelper.class.getName();

    @NonNull
    public static Pair<List<String>, Map<String, OfflineRegion>> getOfflineRegionInfo (final OfflineRegion[] offlineRegions) {
        List<String> offlineRegionNames = new ArrayList<>();
        Map<String, OfflineRegion> modelMap = new HashMap<>();

        for(int position = 0; position < offlineRegions.length; position++) {

            byte[] metadataBytes = offlineRegions[position].getMetadata();
            try {
                JSONObject jsonObject = new JSONObject(new String(metadataBytes));
                if (jsonObject.has(MapBoxOfflineResourcesDownloader.METADATA_JSON_FIELD_REGION_NAME)) {
                    String regionName = jsonObject.getString(MapBoxOfflineResourcesDownloader.METADATA_JSON_FIELD_REGION_NAME);
                    offlineRegionNames.add(regionName);
                    modelMap.put(regionName, offlineRegions[position]);
                }

            } catch (JSONException e) {
                LogUtil.e(TAG, e);
            }

        }

        return new Pair(offlineRegionNames, modelMap);
    }
}
