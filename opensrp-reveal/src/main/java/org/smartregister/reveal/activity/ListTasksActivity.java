package org.smartregister.reveal.activity;

import android.content.Context;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.support.design.widget.NavigationView;
import android.support.v4.view.GravityCompat;
import android.support.v4.widget.DrawerLayout;
import android.util.Log;
import android.view.View;
import android.widget.ImageButton;
import android.widget.LinearLayout.LayoutParams;
import android.widget.TextView;

import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.OnMapReadyCallback;

import org.smartregister.AllConstants;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.ListTaskView;
import org.smartregister.reveal.presenter.ListTaskPresenter;
import org.smartregister.util.Utils;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/**
 * Created by samuelgithengi on 11/20/18.
 */
public class ListTasksActivity extends BaseMapActivity implements ListTaskView {

    private static final String TAG = "ListTasksActivity";

    private DrawerLayout mDrawerLayout;

    private AllSharedPreferences sharedPreferences;

    private ListTaskPresenter listTaskPresenter;

    private TextView campaignTextView;
    private TextView operationalAreaTextView;
    private TextView districtTextView;
    private TextView facilityTextView;
    private TextView operatorTextView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_list_tasks);

        listTaskPresenter = new ListTaskPresenter(this);

        kujakuMapView = findViewById(R.id.kujakuMapView);
        kujakuMapView.onCreate(savedInstanceState);
        kujakuMapView.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(MapboxMap mapboxMap) {
                // Customize map with markers, polylines, etc.
            }
        });

        mDrawerLayout = findViewById(R.id.drawer_layout);

        ImageButton mDrawerMenuButton = findViewById(R.id.drawerMenu);
        mDrawerMenuButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mDrawerLayout.openDrawer(GravityCompat.START);
            }
        });

        sharedPreferences = RevealApplication.getInstance().getContext().allSharedPreferences();

        initializeDrawerLayout();


    }

    private void initializeDrawerLayout() {
        NavigationView navigationView = findViewById(R.id.nav_view);
        View headerView = navigationView.getHeaderView(0);
        int screenHeightPixels = getResources().getDisplayMetrics().heightPixels
                - getResources().getDimensionPixelSize(R.dimen.drawer_margin_vertical);
        headerView.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, screenHeightPixels));

        try {
            ((TextView) headerView.findViewById(R.id.application_version))
                    .setText(getString(R.string.app_version, Utils.getVersion(this)));
        } catch (PackageManager.NameNotFoundException e) {
            Log.e(TAG, e.getMessage(), e);
        }

        String buildDate = new SimpleDateFormat("dd/MM/yyyy", Locale.getDefault())
                .format(new Date(AllConstants.BUILD_TIMESTAMP));
        ((TextView) headerView.findViewById(R.id.application_updated)).setText(buildDate);

        campaignTextView = headerView.findViewById(R.id.campaign_selector);
        operationalAreaTextView = headerView.findViewById(R.id.operational_area_selector);
        districtTextView = headerView.findViewById(R.id.district_label);
        facilityTextView = headerView.findViewById(R.id.facility_label);
        operatorTextView = headerView.findViewById(R.id.operator_label);

        listTaskPresenter.onInitializeDrawerLayout();
        setOperator();
    }

    @Override
    public void setCampaign(String campaign) {
        campaignTextView.setText(campaign);
    }

    @Override
    public void setOperationalArea(String operationalArea) {
        operationalAreaTextView.setText(operationalArea);
    }

    @Override
    public void setDistrict(String district) {
        org.smartregister.reveal.util.Utils.setTextViewText(districtTextView, R.string.district, district);
    }

    @Override
    public void setFacility(String facility) {
        org.smartregister.reveal.util.Utils.setTextViewText(facilityTextView, R.string.facility, facility);
    }

    public void setOperator() {
        org.smartregister.reveal.util.Utils.setTextViewText(operatorTextView, R.string.operator, sharedPreferences.fetchRegisteredANM());
    }


    @Override
    public Context getContext() {
        return this;
    }
}
