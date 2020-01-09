package org.smartregister.reveal.view;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.design.widget.TabLayout;
import android.support.v4.view.ViewPager;
import android.support.v7.app.AppCompatActivity;

import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.OfflineMapDownloadCallback;
import org.smartregister.reveal.fragment.AvailableOfflineMapsFragment;
import org.smartregister.reveal.fragment.DownloadedOfflineMapsFragment;
import org.smartregister.reveal.model.OfflineMapModel;

public class OfflineMapsActivity extends AppCompatActivity implements OfflineMapDownloadCallback {

    private ViewPagerAdapter adapter;

    private static  final int AVAILABLE_OFFLINE_MAPS_FRAGMENT_INDEX = 0;
    private static  final int DOWNLOADED_OFFLINE_MAPS_FRAGMENT_INDEX = 1;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_offline_maps);

        setupViews();
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

        DownloadedOfflineMapsFragment downloadedOfflineMapsFragment = DownloadedOfflineMapsFragment.newInstance(this.getIntent().getExtras());
        downloadedOfflineMapsFragment.setOfflineMapDownloadCallback(this);
        adapter.addFragment(downloadedOfflineMapsFragment, this.getString(R.string.downloaded).toUpperCase());

        viewPager.setAdapter(adapter);

        return viewPager;
    }

    @Override
    public void onMapDownloaded(OfflineMapModel offlineMapModel) {
        DownloadedOfflineMapsFragment downloadedOfflineMapsFragment = (DownloadedOfflineMapsFragment)  adapter.getItem(DOWNLOADED_OFFLINE_MAPS_FRAGMENT_INDEX);
        downloadedOfflineMapsFragment.updateDownloadedMapsList(offlineMapModel);
    }

    @Override
    public void onOfflineMapDeleted(OfflineMapModel offlineMapModel) {
        AvailableOfflineMapsFragment availableOfflineMapsFragment = (AvailableOfflineMapsFragment)  adapter.getItem(AVAILABLE_OFFLINE_MAPS_FRAGMENT_INDEX);
        availableOfflineMapsFragment.updateOperationalAreasToDownload(offlineMapModel);
    }
}
