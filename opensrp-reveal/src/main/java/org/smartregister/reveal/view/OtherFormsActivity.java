package org.smartregister.reveal.view;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.design.widget.TabLayout;
import android.support.v4.view.ViewPager;
import android.support.v7.app.AppCompatActivity;

import org.json.JSONObject;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.OtherFormsContract;
import org.smartregister.reveal.fragment.OtherFormsFragment;
import org.smartregister.reveal.presenter.OtherFormsPresenter;
import org.smartregister.reveal.util.RevealJsonFormUtils;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.JSON_FORM_PARAM_JSON;
import static org.smartregister.reveal.util.Constants.REQUEST_CODE_GET_JSON;

public class OtherFormsActivity extends AppCompatActivity implements OtherFormsContract.View {

    private OtherFormsPresenter presenter;
    private OtherFormsFragment otherFormsFragment;
    private RevealJsonFormUtils jsonFormUtils;
    private ViewPagerAdapter adapter;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_other_forms);
        presenter = new OtherFormsPresenter(this);
        setupViews();
    }

    protected void setupViews() {
        TabLayout tabLayout = findViewById(R.id.tabs);
        ViewPager viewPager = findViewById(R.id.viewpager);
        tabLayout.setupWithViewPager(setupViewPager(viewPager));
    }

    protected ViewPager setupViewPager(ViewPager viewPager) {
        adapter = new ViewPagerAdapter(getSupportFragmentManager());

        jsonFormUtils = new RevealJsonFormUtils();

        otherFormsFragment = OtherFormsFragment.newInstance(this.getIntent().getExtras());
        otherFormsFragment.setJsonFormUtils(jsonFormUtils);
        adapter.addFragment(otherFormsFragment, this.getString(R.string.other_forms).toUpperCase());


        viewPager.setAdapter(adapter);

        return viewPager;
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        if (requestCode == REQUEST_CODE_GET_JSON && resultCode == RESULT_OK && data.hasExtra(JSON_FORM_PARAM_JSON)) {
            String json = data.getStringExtra(JSON_FORM_PARAM_JSON);
            Timber.d( json);
            getPresenter().saveJsonForm(json);
        }
    }

    @Override
    public void startFormActivity(JSONObject jsonObject) {
        jsonFormUtils.startJsonForm(jsonObject, this);
    }

    @Override
    public void saveJsonForm(String json) {

    }

    private OtherFormsContract.Presenter getPresenter() {
        return (OtherFormsContract.Presenter) presenter;
    }
}
