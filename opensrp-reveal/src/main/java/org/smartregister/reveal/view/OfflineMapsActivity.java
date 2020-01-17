package org.smartregister.reveal.view;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.design.widget.TabLayout;
import android.support.v4.util.Pair;
import android.support.v4.view.ViewPager;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.View;

import com.mapbox.mapboxsdk.Mapbox;
import com.mapbox.mapboxsdk.offline.OfflineManager;
import com.mapbox.mapboxsdk.offline.OfflineRegion;

import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.OfflineMapDownloadCallback;
import org.smartregister.reveal.fragment.AvailableOfflineMapsFragment;
import org.smartregister.reveal.fragment.DownloadedOfflineMapsFragment;
import org.smartregister.reveal.model.OfflineMapModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.ona.kujaku.downloaders.MapBoxOfflineResourcesDownloader;
import io.ona.kujaku.utils.LogUtil;

public class OfflineMapsActivity extends AppCompatActivity implements OfflineMapDownloadCallback {

    private static final String TAG = OfflineMapsActivity.class.getName();

    private ViewPagerAdapter adapter;

    private static  final int AVAILABLE_OFFLINE_MAPS_FRAGMENT_INDEX = 0;
    private static  final int DOWNLOADED_OFFLINE_MAPS_FRAGMENT_INDEX = 1;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_offline_maps);

        setUpToolbar();

        setupViews();

        getOfflineDownloadedRegions(false);
    }

    protected void setUpToolbar() {
        Toolbar toolbar= this.findViewById(R.id.offline_maps_toolbar);
        toolbar.setTitle(R.string.offline_maps);
        this.setSupportActionBar(toolbar);
        this.getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        toolbar.setNavigationOnClickListener(new View.OnClickListener(){

            @Override
            public void onClick(View v) {
                onBackPressed();
            }
        });
    }

    protected void setupViews() {
        TabLayout tabLayout = findViewById(R.id.tabs);
        ViewPager viewPager = findViewById(R.id.viewpager);
        tabLayout.setupWithViewPager(setupViewPager(viewPager));
    }

    protected ViewPager setupViewPager(ViewPager viewPager) {
        adapter = new ViewPagerAdapter(getSupportFragmentManager());

        AvailableOfflineMapsFragment availableOfflineMapsFragment = AvailableOfflineMapsFragment.newInstance(this.getIntent().getExtras());
        availableOfflineMapsFragment.setOfflineMapDownloadCallback(this);
        adapter.addFragment(availableOfflineMapsFragment, this.getString(R.string.available).toUpperCase());

        DownloadedOfflineMapsFragment downloadedOfflineMapsFragment = DownloadedOfflineMapsFragment.newInstance(this.getIntent().getExtras(), this );
        downloadedOfflineMapsFragment.setOfflineMapDownloadCallback(this);
        adapter.addFragment(downloadedOfflineMapsFragment, this.getString(R.string.downloaded).toUpperCase());

        viewPager.setAdapter(adapter);

        return viewPager;
    }

    @Override
    public void onMapDownloaded(OfflineMapModel offlineMapModel) {
        getOfflineDownloadedRegions(true);
    }

    @Override
    public void onOfflineMapDeleted(OfflineMapModel offlineMapModel) {
        AvailableOfflineMapsFragment availableOfflineMapsFragment = (AvailableOfflineMapsFragment)  adapter.getItem(AVAILABLE_OFFLINE_MAPS_FRAGMENT_INDEX);
        availableOfflineMapsFragment.updateOperationalAreasToDownload(offlineMapModel);
    }

    private void getOfflineDownloadedRegions(boolean refreshDownloadedListOnly) {
        Mapbox.getInstance(this, BuildConfig.MAPBOX_SDK_ACCESS_TOKEN);

        OfflineManager offlineManager = OfflineManager.getInstance(this);
        offlineManager.listOfflineRegions(new OfflineManager.ListOfflineRegionsCallback() {
            @Override
            public void onList(final OfflineRegion[] offlineRegions) {
                Pair<List<String>, Map<String, OfflineRegion>>  offlineRegionInfo = null;
                if (offlineRegions != null && offlineRegions.length > 0) {
                    offlineRegionInfo = getOfflineRegionInfo(offlineRegions);
                }
                setOfflineDownloadedMapNames(offlineRegionInfo, refreshDownloadedListOnly);
            }

            @Override
            public void onError(String error) {
                Log.e(TAG, "ERROR :: "  + error);
            }
        });
    }

    @NonNull
    private Pair<List<String>, Map<String, OfflineRegion>> getOfflineRegionInfo (final OfflineRegion[] offlineRegions) {
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

    private void setOfflineDownloadedMapNames(Pair<List<String>, Map<String, OfflineRegion>>  offlineRegionInfo, boolean refreshDownloadedListOnly) {
        DownloadedOfflineMapsFragment downloadedOfflineMapsFragment = (DownloadedOfflineMapsFragment)  adapter.getItem(DOWNLOADED_OFFLINE_MAPS_FRAGMENT_INDEX);
        downloadedOfflineMapsFragment.setOfflineDownloadedMapNames(offlineRegionInfo);

        if (refreshDownloadedListOnly){
            return;
        }

        AvailableOfflineMapsFragment availableOfflineMapsFragment = (AvailableOfflineMapsFragment)  adapter.getItem(AVAILABLE_OFFLINE_MAPS_FRAGMENT_INDEX);
        List<String> regionNames = offlineRegionInfo != null ? offlineRegionInfo.first : null;
        availableOfflineMapsFragment.setOfflineDownloadedMapNames(regionNames);

    }
}
