package org.smartregister.reveal.view;

import android.os.Bundle;
import android.view.View;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.Toolbar;
import androidx.viewpager.widget.ViewPager;

import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.LocationPickerFragment;
import org.smartregister.view.activity.MultiLanguageActivity;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class LocationPickerActivity extends MultiLanguageActivity {

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_offline_maps);

        Toolbar toolbar = this.findViewById(R.id.offline_maps_toolbar);
        this.setSupportActionBar(toolbar);
        this.getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                onBackPressed();
            }
        });

        setupViews();
    }

    protected void setupViews() {
        ViewPager viewPager = findViewById(R.id.viewpager);
        setupViewPager(viewPager);
    }

    protected ViewPager setupViewPager(ViewPager viewPager) {
        ViewPagerAdapter adapter = new ViewPagerAdapter(getSupportFragmentManager());

        LocationPickerFragment locationPickerFragment = LocationPickerFragment.newInstance(this.getIntent().getExtras());
        adapter.addFragment(locationPickerFragment, this.getString(R.string.p2p_locations).toUpperCase());

        viewPager.setAdapter(adapter);

        return viewPager;
    }
}
