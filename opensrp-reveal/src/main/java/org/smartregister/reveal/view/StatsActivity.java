package org.smartregister.reveal.view;

import android.app.ProgressDialog;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.view.ViewPager;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.support.v7.widget.Toolbar;

import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.StatsContract;
import org.smartregister.reveal.fragment.StatsFragment;
import org.smartregister.reveal.presenter.StatsPresenter;

public class StatsActivity extends AppCompatActivity implements StatsContract.View {
    private StatsPresenter presenter;
    private ProgressDialog progressDialog;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_stats);
        presenter = new StatsPresenter();

        Toolbar toolbar = this.findViewById(R.id.summary_toolbar);
        toolbar.setTitle(R.string.return_to_register);
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


        StatsFragment statsFragment = StatsFragment.newInstance(this.getIntent().getExtras());
        adapter.addFragment(statsFragment, this.getString(R.string.summary_forms).toUpperCase());


        viewPager.setAdapter(adapter);

        return viewPager;
    }

    @Override
    public void showProgressDialog(int titleIdentifier) {
        progressDialog = new ProgressDialog(this);
        progressDialog.setCancelable(false);
        progressDialog.setTitle(titleIdentifier);
        progressDialog.setMessage(getString(R.string.please_wait_message));
        if (!isFinishing())
            progressDialog.show();
    }

    @Override
    public void hideProgressDialog() {
        if (progressDialog != null) {
            progressDialog.dismiss();
        }
    }

}
