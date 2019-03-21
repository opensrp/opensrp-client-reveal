package org.smartregister.reveal.view;

import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.res.Configuration;
import android.location.Location;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.StringRes;
import android.support.design.widget.Snackbar;
import android.support.v4.content.LocalBroadcastManager;
import android.support.v7.app.AlertDialog;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.CardView;
import android.text.TextUtils;
import android.util.Log;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageButton;
import android.widget.TextView;
import android.widget.Toast;

import com.mapbox.android.core.permissions.PermissionsManager;
import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.geojson.Geometry;
import com.mapbox.mapboxsdk.camera.CameraPosition;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.OnMapReadyCallback;
import com.mapbox.mapboxsdk.style.sources.GeoJsonSource;

import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.domain.FetchStatus;
import org.smartregister.receiver.SyncStatusBroadcastReceiver;
import org.smartregister.reveal.R;
import org.smartregister.reveal.activity.RevealJsonFormActivity;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.contract.UserLocationContract.UserLocationView;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.presenter.ListTaskPresenter;
import org.smartregister.reveal.util.Constants.Action;
import org.smartregister.reveal.util.Constants.TaskRegister;
import org.smartregister.reveal.util.RevealMapHelper;

import io.ona.kujaku.callbacks.OnLocationComponentInitializedCallback;
import io.ona.kujaku.utils.Constants;

import static org.smartregister.reveal.util.Constants.ANIMATE_TO_LOCATION_DURATION;
import static org.smartregister.reveal.util.Constants.CONFIGURATION.UPDATE_LOCATION_BUFFER_RADIUS;
import static org.smartregister.reveal.util.Constants.JSON_FORM_PARAM_JSON;
import static org.smartregister.reveal.util.Constants.REQUEST_CODE_GET_JSON;
import static org.smartregister.reveal.util.Constants.VERTICAL_OFFSET;
import static org.smartregister.reveal.util.FamilyConstants.Intent.START_REGISTRATION;


/**
 * Created by samuelgithengi on 11/20/18.
 */
public class ListTasksActivity extends BaseMapActivity implements ListTaskContract.ListTaskView,
        View.OnClickListener, SyncStatusBroadcastReceiver.SyncStatusListener, UserLocationView, OnLocationComponentInitializedCallback {

    private static final String TAG = "ListTasksActivity";

    private ListTaskPresenter listTaskPresenter;

    private View rootView;

    private GeoJsonSource geoJsonSource;

    private GeoJsonSource selectedGeoJsonSource;

    private ProgressDialog progressDialog;

    private MapboxMap mMapboxMap;

    private CardView structureInfoCardView;
    private TextView tvSprayStatus;
    private TextView tvPropertyType;
    private TextView tvSprayDate;
    private TextView tvSprayOperator;
    private TextView tvFamilyHead;
    private TextView tvReason;

    private RefreshGeowidgetReceiver refreshGeowidgetReceiver = new RefreshGeowidgetReceiver();

    private boolean hasRequestedLocation;

    private Snackbar syncProgressSnackbar;

    private BaseDrawerContract.View drawerView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_list_tasks);

        drawerView = new DrawerMenuView(this);

        listTaskPresenter = new ListTaskPresenter(this, drawerView.getPresenter());
        rootView = findViewById(R.id.content_frame);

        initializeMapView(savedInstanceState);

        drawerView.initializeDrawerLayout();
        initializeProgressDialog();

        findViewById(R.id.btn_add_structure).setOnClickListener(this);
        findViewById(R.id.drawerMenu).setOnClickListener(this);

        initializeCardView();

        syncProgressSnackbar = Snackbar.make(rootView, getString(org.smartregister.R.string.syncing), Snackbar.LENGTH_INDEFINITE);
    }

    private void initializeCardView() {
        structureInfoCardView = findViewById(R.id.structure_info_card_view);
        structureInfoCardView.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(View v, MotionEvent event) {
                //intercept clicks and interaction of map below card view
                return true;
            }
        });
        findViewById(R.id.btn_add_structure).setOnClickListener(this);

        findViewById(R.id.btn_collapse_structure_card_view).setOnClickListener(this);


        tvSprayStatus = findViewById(R.id.spray_status);
        tvPropertyType = findViewById(R.id.property_type);
        tvSprayDate = findViewById(R.id.spray_date);
        tvSprayOperator = findViewById(R.id.user_id);
        tvFamilyHead = findViewById(R.id.family_head);
        tvReason = findViewById(R.id.reason);
        findViewById(R.id.change_spray_status).setOnClickListener(this);

        findViewById(R.id.register_family).setOnClickListener(this);

        findViewById(R.id.task_register).setOnClickListener(this);

    }

    @Override
    public void closeStructureCardView() {
        setViewVisibility(structureInfoCardView, false);
    }

    private void setViewVisibility(View view, boolean isVisible) {
        view.setVisibility(isVisible ? View.VISIBLE : View.GONE);
    }

    private void initializeMapView(Bundle savedInstanceState) {
        kujakuMapView = findViewById(R.id.kujakuMapView);

        kujakuMapView.getMapboxLocationComponentWrapper().setOnLocationComponentInitializedCallback(this);

        kujakuMapView.onCreate(savedInstanceState);

        kujakuMapView.showCurrentLocationBtn(true);

        Float locationBufferRadius = org.smartregister.reveal.util.Utils.getLocationBuffer();
        kujakuMapView.setLocationBufferRadius(locationBufferRadius);

        kujakuMapView.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(MapboxMap mapboxMap) {
                mMapboxMap = mapboxMap;

                RevealMapHelper.addSymbolLayers(mapboxMap, ListTasksActivity.this);

                mapboxMap.setMinZoomPreference(10);
                mapboxMap.setMaxZoomPreference(21);

                CameraPosition cameraPosition = new CameraPosition.Builder()
                        .zoom(16)
                        .build();
                mapboxMap.setCameraPosition(cameraPosition);

                geoJsonSource = mapboxMap.getSourceAs(getString(R.string.reveal_datasource_name));

                selectedGeoJsonSource = mapboxMap.getSourceAs(getString(R.string.selected_datasource_name));

                listTaskPresenter.onMapReady();

                mapboxMap.addOnMapClickListener(new MapboxMap.OnMapClickListener() {
                    @Override
                    public void onMapClick(@NonNull LatLng point) {
                        listTaskPresenter.onMapClicked(mapboxMap, point);
                    }
                });

                displayMyLocationAtButtom();
            }
        });
    }

    private void displayMyLocationAtButtom() {
        ImageButton myLocationComponent = findViewById(R.id.ib_mapview_focusOnMyLocationIcon);
        if (myLocationComponent != null) {
            FrameLayout.LayoutParams params = (FrameLayout.LayoutParams) myLocationComponent.getLayoutParams();
            params.gravity = Gravity.BOTTOM | Gravity.END;
            params.bottomMargin = params.topMargin;
            params.topMargin = 0;
            myLocationComponent.setLayoutParams(params);
        }
    }

    @Override
    public void onClick(View v) {
        if (v.getId() == R.id.btn_add_structure) {
            listTaskPresenter.onAddStructureClicked();
        } else if (v.getId() == R.id.change_spray_status) {
            listTaskPresenter.onChangeSprayStatus();
        } else if (v.getId() == R.id.btn_collapse_structure_card_view) {
            setViewVisibility(tvReason, false);
            closeStructureCardView();
        } else if (v.getId() == R.id.register_family) {
            registerFamily();
        } else if (v.getId() == R.id.task_register) {
            openTaskRegister();
        } else if (v.getId() == R.id.drawerMenu) {
            drawerView.openDrawerLayout();
        }
    }

    private void openTaskRegister() {
        Intent intent = new Intent(this, TaskRegisterActivity.class);
        intent.putExtra(TaskRegister.INTERVENTION_TYPE, getIntent().getStringExtra(TaskRegister.INTERVENTION_TYPE));
        if (getUserCurrentLocation() != null) {
            intent.putExtra(TaskRegister.LAST_USER_LOCATION, getUserCurrentLocation());
        }
        startActivity(intent);
    }

    private void registerFamily() {
        Intent intent = new Intent(this, FamilyRegisterActivity.class);
        intent.putExtra(START_REGISTRATION, true);
        startActivity(intent);
    }

    @Override
    public void onLocationComponentInitialized() {
        if (PermissionsManager.areLocationPermissionsGranted(this)) {
            kujakuMapView.getMapboxLocationComponentWrapper()
                    .getLocationComponent()
                    .applyStyle(getApplicationContext(), R.style.LocationComponentStyling);
        }
    }

    @Override
    public void setGeoJsonSource(@NonNull FeatureCollection featureCollection, Geometry operationalAreaGeometry) {
        if (geoJsonSource != null) {
            geoJsonSource.setGeoJson(featureCollection);
            if (operationalAreaGeometry != null) {
                mMapboxMap.setCameraPosition(mMapboxMap.getCameraForGeometry(operationalAreaGeometry));
            }
        }
    }

    @Override
    public void displayNotification(int title, int message, Object... formatArgs) {
        if (formatArgs.length == 0)
            new AlertDialog.Builder(this).setMessage(message).setTitle(title).setPositiveButton(R.string.ok, null).show();
        else
            new AlertDialog.Builder(this).setMessage(getString(message, formatArgs)).setTitle(title).setPositiveButton(R.string.ok, null).show();
    }

    @Override
    public void displayNotification(String message) {
        new AlertDialog.Builder(this).setMessage(message).setTitle(R.string.fetch_structures_title).setPositiveButton(R.string.ok, null).show();
    }

    @Override
    public void openCardView(CardDetails cardDetails) {

        tvSprayStatus.setTextColor(getResources().getColor(cardDetails.getStatusColor()));
        tvSprayStatus.setText(cardDetails.getStatusMessage());
        tvPropertyType.setText(cardDetails.getPropertyType());
        tvSprayDate.setText(cardDetails.getSprayDate());
        tvSprayOperator.setText(cardDetails.getSprayOperator());
        tvFamilyHead.setText(cardDetails.getFamilyHead());
        if (!TextUtils.isEmpty(cardDetails.getReason())) {
            tvReason.setVisibility(View.VISIBLE);
            tvReason.setText(cardDetails.getReason());
        } else {
            tvReason.setVisibility(View.GONE);
        }

        structureInfoCardView.setVisibility(View.VISIBLE);
    }

    @Override
    public void startJsonForm(JSONObject form) {
        Intent intent = new Intent(getApplicationContext(), RevealJsonFormActivity.class);
        try {
            intent.putExtra(JSON_FORM_PARAM_JSON, form.toString());
            startActivityForResult(intent, REQUEST_CODE_GET_JSON);
        } catch (Exception e) {
            Log.e(TAG, e.getMessage());
        }
    }

    @Override
    public void displaySelectedFeature(Feature feature, LatLng point) {
        adjustFocusPoint(point);
        kujakuMapView.centerMap(point, ANIMATE_TO_LOCATION_DURATION, mMapboxMap.getCameraPosition().zoom);
        if (selectedGeoJsonSource != null) {
            selectedGeoJsonSource.setGeoJson(FeatureCollection.fromFeature(feature));
        }
    }

    private void adjustFocusPoint(LatLng point) {
        int screenSize = getResources().getConfiguration().screenLayout & Configuration.SCREENLAYOUT_SIZE_MASK;
        if (screenSize == Configuration.SCREENLAYOUT_SIZE_NORMAL || screenSize == Configuration.SCREENLAYOUT_SIZE_SMALL) {
            point.setLatitude(point.getLatitude() + VERTICAL_OFFSET);
        }
    }

    @Override
    public void clearSelectedFeature() {
        if (selectedGeoJsonSource != null) {
            try {
                selectedGeoJsonSource.setGeoJson(new com.cocoahero.android.geojson.FeatureCollection().toJSON().toString());
            } catch (JSONException e) {
                Log.e(TAG, "Error clearing selected feature");
            }
        }
    }

    @Override
    public void displayToast(@StringRes int resourceId) {
        Toast.makeText(this, resourceId, Toast.LENGTH_SHORT).show();
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == REQUEST_CODE_GET_JSON && resultCode == RESULT_OK && data.hasExtra(JSON_FORM_PARAM_JSON)) {
            String json = data.getStringExtra(JSON_FORM_PARAM_JSON);
            Log.d(TAG, json);
            listTaskPresenter.saveJsonForm(json);
        } else if (requestCode == Constants.RequestCode.LOCATION_SETTINGS && hasRequestedLocation) {
            if (resultCode == RESULT_OK) {
                listTaskPresenter.getLocationPresenter().waitForUserLocation();
            } else if (resultCode == RESULT_CANCELED) {
                listTaskPresenter.getLocationPresenter().onGetUserLocationFailed();
            }
            hasRequestedLocation = false;
        }
    }

    private void initializeProgressDialog() {
        progressDialog = new ProgressDialog(this);
        progressDialog.setCancelable(false);
        progressDialog.setTitle(R.string.fetching_structures_title);
        progressDialog.setMessage(getString(R.string.fetching_structures_message));
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

    @Override
    public Location getUserCurrentLocation() {
        return kujakuMapView.getLocationClient() == null ? null : kujakuMapView.getLocationClient().getLastLocation();
    }

    @Override
    public void requestUserLocation() {
        kujakuMapView.setWarmGps(true, getString(R.string.location_service_disabled), getString(R.string.location_services_disabled_spray));
        hasRequestedLocation = true;
    }

    @Override
    public Context getContext() {
        return this;
    }

    @Override
    protected void onDestroy() {
        listTaskPresenter = null;
        super.onDestroy();
    }

    @Override
    public void onSyncStart() {
        if (SyncStatusBroadcastReceiver.getInstance().isSyncing()) {
            syncProgressSnackbar.show();
        }
    }

    @Override
    public void onSyncInProgress(FetchStatus fetchStatus) {
        syncProgressSnackbar.dismiss();
        if (fetchStatus.equals(FetchStatus.fetchedFailed)) {
            Snackbar.make(rootView, org.smartregister.R.string.sync_failed, Snackbar.LENGTH_LONG).show();
        } else if (fetchStatus.equals(FetchStatus.fetched)
                || fetchStatus.equals(FetchStatus.nothingFetched)) {
            Snackbar.make(rootView, org.smartregister.R.string.sync_complete, Snackbar.LENGTH_LONG).show();
        } else if (fetchStatus.equals(FetchStatus.noConnection)) {
            Snackbar.make(rootView, org.smartregister.R.string.sync_failed_no_internet, Snackbar.LENGTH_LONG).show();
        }
    }

    @Override
    public void onSyncComplete(FetchStatus fetchStatus) {
        onSyncInProgress(fetchStatus);
    }

    @Override
    public void onResume() {
        super.onResume();
        SyncStatusBroadcastReceiver.getInstance().addSyncStatusListener(this);
        IntentFilter filter = new IntentFilter(Action.STRUCTURE_TASK_SYNCHED);
        LocalBroadcastManager.getInstance(getApplicationContext()).registerReceiver(refreshGeowidgetReceiver, filter);
        drawerView.onInitializeDrawerLayout();
    }

    @Override
    public void onPause() {
        SyncStatusBroadcastReceiver.getInstance().removeSyncStatusListener(this);
        LocalBroadcastManager.getInstance(getApplicationContext()).unregisterReceiver(refreshGeowidgetReceiver);
        super.onPause();
    }

    @Override
    public void onDrawerClosed() {
        listTaskPresenter.onDrawerClosed();
    }

    @Override
    public AppCompatActivity getActivity() {
        return this;
    }

    private class RefreshGeowidgetReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            Bundle extras = intent.getExtras();
            if (extras != null && extras.getBoolean(UPDATE_LOCATION_BUFFER_RADIUS)) {
                float bufferRadius = org.smartregister.reveal.util.Utils.getLocationBuffer();
                kujakuMapView.setLocationBufferRadius(bufferRadius);
            } else {
                listTaskPresenter.refreshStructures();
            }
        }
    }
}
