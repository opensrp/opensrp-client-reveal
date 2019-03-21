package org.smartregister.reveal.view;

import android.app.ProgressDialog;
import android.content.DialogInterface;
import android.content.pm.PackageManager;
import android.support.annotation.NonNull;
import android.support.annotation.StringRes;
import android.support.constraint.ConstraintLayout;
import android.support.design.widget.NavigationView;
import android.support.v4.util.Pair;
import android.support.v4.view.GravityCompat;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.View;
import android.view.ViewTreeObserver;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.vijay.jsonwizard.customviews.TreeViewDialog;

import org.json.JSONArray;
import org.json.JSONException;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.util.Utils;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

/**
 * Created by samuelgithengi on 3/21/19.
 */
public abstract class BaseDrawerActivity extends AppCompatActivity implements View.OnClickListener, BaseDrawerContract.View {

    private static final String TAG = "BaseDrawerActivity";

    private TextView campaignTextView;
    private TextView operationalAreaTextView;
    private TextView districtTextView;
    private TextView facilityTextView;
    private TextView operatorTextView;

    private DrawerLayout mDrawerLayout;

    private ProgressDialog progressDialog;

    protected void initializeDrawerLayout() {

        mDrawerLayout = findViewById(R.id.drawer_layout);

        ImageButton mDrawerMenuButton = findViewById(R.id.drawerMenu);
        mDrawerMenuButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mDrawerLayout.openDrawer(GravityCompat.START);
            }
        });

        mDrawerLayout.addDrawerListener(new DrawerLayout.DrawerListener() {
            @Override
            public void onDrawerSlide(@NonNull View drawerView, float slideOffset) {//do nothing
            }

            @Override
            public void onDrawerOpened(@NonNull View drawerView) {//do nothing
            }

            @Override
            public void onDrawerClosed(@NonNull View drawerView) {
                getPresenter().onDrawerClosed();
            }

            @Override
            public void onDrawerStateChanged(int newState) {//do nothing
            }
        });

        NavigationView navigationView = findViewById(R.id.nav_view);
        View headerView = navigationView.getHeaderView(0);

        headerView.getViewTreeObserver().addOnGlobalLayoutListener(new ViewTreeObserver.OnGlobalLayoutListener() {
            @Override
            public void onGlobalLayout() {
                headerView.getViewTreeObserver().removeOnGlobalLayoutListener(this);
                int minimumOperatorMargin = getResources().getDimensionPixelSize(R.dimen.operator_top_margin);
                int screenHeightPixels = getResources().getDisplayMetrics().heightPixels;
                //if content of hamburger menu is bigger than screen; scroll content
                if (screenHeightPixels < headerView.getHeight() + minimumOperatorMargin) {
                    headerView.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT));
                    View operator = headerView.findViewById(R.id.operator_label);
                    ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams) operator.getLayoutParams();
                    params.height = ConstraintLayout.LayoutParams.WRAP_CONTENT;
                    operator.setLayoutParams(params);
                } else {//content of hamburger menu fits on screen; set menu height to screen height
                    headerView.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT,
                            screenHeightPixels - getResources().getDimensionPixelSize(R.dimen.hamburger_margin)));
                }
            }
        });

        try {
            ((TextView) headerView.findViewById(R.id.application_version))
                    .setText(getString(R.string.app_version, Utils.getVersion(this)));
        } catch (PackageManager.NameNotFoundException e) {
            Log.e(TAG, e.getMessage(), e);
        }

        String buildDate = new SimpleDateFormat("dd/MM/yyyy", Locale.getDefault())
                .format(new Date(BuildConfig.BUILD_TIMESTAMP));
        ((TextView) headerView.findViewById(R.id.application_updated)).setText(getString(R.string.app_updated, buildDate));

        campaignTextView = headerView.findViewById(R.id.campaign_selector);
        operationalAreaTextView = headerView.findViewById(R.id.operational_area_selector);
        districtTextView = headerView.findViewById(R.id.district_label);
        facilityTextView = headerView.findViewById(R.id.facility_label);
        operatorTextView = headerView.findViewById(R.id.operator_label);

        operationalAreaTextView.setOnClickListener(this);

        campaignTextView.setOnClickListener(this);

        headerView.findViewById(R.id.logout_button).setOnClickListener(this);
        headerView.findViewById(R.id.sync_button).setOnClickListener(this);

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

    @Override
    public void setOperator() {
        org.smartregister.reveal.util.Utils.setTextViewText(operatorTextView, R.string.operator,
                RevealApplication.getInstance().getContext().allSharedPreferences().fetchRegisteredANM());
    }

    @Override
    public void lockNavigationDrawerForSelection() {
        mDrawerLayout.openDrawer(GravityCompat.START);
        mDrawerLayout.setDrawerLockMode(DrawerLayout.LOCK_MODE_LOCKED_OPEN);

    }

    @Override
    public void unlockNavigationDrawer() {
        if (mDrawerLayout.getDrawerLockMode(GravityCompat.START) == DrawerLayout.LOCK_MODE_LOCKED_OPEN) {
            mDrawerLayout.closeDrawer(GravityCompat.START);
            mDrawerLayout.setDrawerLockMode(DrawerLayout.LOCK_MODE_UNLOCKED);
        }
    }

    @Override
    public void showOperationalAreaSelector(Pair<String, ArrayList<String>> locationHierarchy) {
        try {
            TreeViewDialog treeViewDialog = new TreeViewDialog(this,
                    R.style.AppTheme_WideDialog,
                    new JSONArray(locationHierarchy.first), locationHierarchy.second, locationHierarchy.second);
            treeViewDialog.setCancelable(true);
            treeViewDialog.setCanceledOnTouchOutside(true);
            treeViewDialog.setOnDismissListener(new DialogInterface.OnDismissListener() {
                @Override
                public void onDismiss(DialogInterface dialog) {
                    getPresenter().onOperationalAreaSelectorClicked(treeViewDialog.getName());
                }
            });
            treeViewDialog.show();
        } catch (JSONException e) {
            Log.e(TAG, e.getMessage());
        }

    }


    @Override
    public void showCampaignSelector(List<String> campaigns, String entireTreeString) {
        try {
            TreeViewDialog treeViewDialog = new TreeViewDialog(this,
                    R.style.AppTheme_WideDialog,
                    new JSONArray(entireTreeString), new ArrayList<>(campaigns), new ArrayList<>(campaigns));
            treeViewDialog.show();
            treeViewDialog.setCanceledOnTouchOutside(true);
            treeViewDialog.setOnDismissListener(new DialogInterface.OnDismissListener() {
                @Override
                public void onDismiss(DialogInterface dialog) {
                    getPresenter().onCampaignSelectorClicked(treeViewDialog.getValue(), treeViewDialog.getName());
                }
            });
            treeViewDialog.show();
        } catch (JSONException e) {
            Log.e(TAG, e.getMessage());
        }
    }

    @Override
    public void showProgressDialog(@StringRes int title, @StringRes int message) {
        if (progressDialog != null) {
            progressDialog.setTitle(title);
            progressDialog.setMessage(getString(message));
            progressDialog.show();
        }
    }

    @Override
    public void hideProgressDialog() {
        if (progressDialog != null) {
            progressDialog.dismiss();
        }
    }

    protected void initializeProgressDialog() {
        progressDialog = new ProgressDialog(this);
        progressDialog.setCancelable(false);
        progressDialog.setTitle(R.string.fetching_structures_title);
        progressDialog.setMessage(getString(R.string.fetching_structures_message));
    }

    protected void closeDrawerLayout() {
        mDrawerLayout.closeDrawer(GravityCompat.START);
    }

    protected abstract BaseDrawerContract.Presenter getPresenter();


}
