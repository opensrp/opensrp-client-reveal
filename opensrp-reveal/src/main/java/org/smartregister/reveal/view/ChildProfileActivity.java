package org.smartregister.reveal.view;

import android.app.Activity;
import android.content.Intent;
import android.graphics.PorterDuff;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.os.Bundle;
import android.support.v4.view.ViewPager;
import android.support.v7.app.ActionBar;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import org.json.JSONObject;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.ChildProfileContract;
import org.smartregister.reveal.interactor.GenericInteractor;
import org.smartregister.reveal.model.Child;
import org.smartregister.reveal.presenter.ChildProfilePresenter;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.view.activity.BaseProfileActivity;

import java.util.concurrent.atomic.AtomicInteger;

import timber.log.Timber;

public class ChildProfileActivity extends BaseProfileActivity implements ChildProfileContract.View {

    protected ProgressBar progressBar;
    protected AtomicInteger incompleteRequests = new AtomicInteger(0);
    private String childBaseEntityID;
    private TextView tvNames;
    private TextView tvNumber;
    private TextView tvGender;
    private TextView tvAge;

    public static void startMe(Activity activity, String childBaseEntityID) {
        Intent intent = new Intent(activity, ChildProfileActivity.class);
        intent.putExtra(Constants.Properties.BASE_ENTITY_ID, childBaseEntityID);
        activity.startActivity(intent);
    }

    @Override
    protected void onCreation() {
        setContentView(R.layout.activity_child_profile);
        Toolbar toolbar = findViewById(R.id.collapsing_toolbar);
        toolbar.findViewById(R.id.toolbar_title).setOnClickListener(v -> onBackPressed());
        setSupportActionBar(toolbar);

        Bundle bundle = getIntent().getExtras();
        if (bundle != null) {
            childBaseEntityID = bundle.getString(Constants.Properties.BASE_ENTITY_ID);
        }

        ActionBar actionBar = getSupportActionBar();
        if (actionBar != null) {
            actionBar.setDisplayHomeAsUpEnabled(true);
            final Drawable upArrow = getResources().getDrawable(R.drawable.ic_arrow_back_white_24dp);
            upArrow.setColorFilter(getResources().getColor(R.color.text_blue), PorterDuff.Mode.SRC_ATOP);
            actionBar.setHomeAsUpIndicator(upArrow);
        }
        toolbar.setNavigationOnClickListener(v -> onBackPressed());
        appBarLayout = findViewById(R.id.collapsing_toolbar_appbarlayout);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            appBarLayout.setOutlineProvider(null);
        }

        initializePresenter();
        setupViews();
        fetchProfileData();
    }

    @Override
    protected void setupViews() {
        tvNames = findViewById(R.id.tvNames);
        tvNumber = findViewById(R.id.tvNumber);
        tvGender = findViewById(R.id.tvGender);
        tvAge = findViewById(R.id.tvAge);
        progressBar = findViewById(R.id.progressBar);
    }

    @Override
    protected void initializePresenter() {
        presenter = new ChildProfilePresenter(this)
                .usingInteractor(new GenericInteractor());
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int i = item.getItemId();
        if (i == R.id.edit_child) {
            startEditForm();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.child_profile_menu, menu);
        return true;
    }

    @Override
    protected ViewPager setupViewPager(ViewPager viewPager) {
        return null;
    }

    @Override
    protected void fetchProfileData() {
        getPresenter().fetchProfileData(childBaseEntityID);
    }


    @Override
    public void onFetchResult(Child child) {
        tvNames.setText(child.getFullName() + ", " + child.getGrade());
        tvNumber.setText(child.getUniqueID());
        tvGender.setText(child.getGender());
        tvAge.setText(getString(R.string.child_age, child.getAge()));
    }

    @Override
    public void setLoadingState(boolean state) {
        int result = state ? incompleteRequests.incrementAndGet() : incompleteRequests.decrementAndGet();
        progressBar.setVisibility(result > 0 ? View.VISIBLE : View.INVISIBLE);
    }

    @Override
    public ChildProfileContract.Presenter getPresenter() {
        return (ChildProfileContract.Presenter) presenter;
    }

    @Override
    public void onError(Exception e) {
        Toast.makeText(getBaseContext(), R.string.an_error_occured, Toast.LENGTH_SHORT).show();
        Timber.e(e);
        finish();
    }

    @Override
    public void startEditForm() {
        getPresenter().startChildRegistrationForm(getBaseContext(), childBaseEntityID);
    }

    @Override
    public void startJsonForm(JSONObject form) {
        new RevealJsonFormUtils().startJsonForm(form, this);
    }
}
