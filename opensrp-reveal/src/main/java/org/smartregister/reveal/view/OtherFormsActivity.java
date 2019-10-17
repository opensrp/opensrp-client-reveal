package org.smartregister.reveal.view;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.design.widget.TabLayout;
import android.support.v4.view.ViewPager;
import android.support.v7.app.AppCompatActivity;

import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.OtherFormsFragment;

public class OtherFormsActivity extends AppCompatActivity {

    private OtherFormsFragment otherFormsFragment;
    private ViewPagerAdapter adapter;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_other_forms);
        setupViews();
    }

    protected void setupViews() {
        TabLayout tabLayout = findViewById(R.id.tabs);
        ViewPager viewPager = findViewById(R.id.viewpager);
        tabLayout.setupWithViewPager(setupViewPager(viewPager));
    }

    protected ViewPager setupViewPager(ViewPager viewPager) {
        adapter = new ViewPagerAdapter(getSupportFragmentManager());

        otherFormsFragment = OtherFormsFragment.newInstance(this.getIntent().getExtras());
        adapter.addFragment(otherFormsFragment, this.getString(R.string.other_forms).toUpperCase());


        viewPager.setAdapter(adapter);

        return viewPager;
    }
}
