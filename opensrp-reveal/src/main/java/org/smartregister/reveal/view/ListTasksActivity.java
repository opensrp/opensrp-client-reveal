package org.smartregister.reveal.view;

import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.res.Configuration;
import android.graphics.Color;
import android.location.Location;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.StringRes;
import android.support.design.widget.Snackbar;
import android.support.v4.content.LocalBroadcastManager;
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
import com.mapbox.mapboxsdk.camera.CameraPosition;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.OnMapReadyCallback;
import com.mapbox.mapboxsdk.maps.Style;
import com.mapbox.mapboxsdk.style.sources.GeoJsonSource;

import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.domain.FetchStatus;
import org.smartregister.receiver.SyncStatusBroadcastReceiver;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.contract.UserLocationContract.UserLocationView;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.MosquitoCollectionCardDetails;
import org.smartregister.reveal.model.SprayCardDetails;
import org.smartregister.reveal.presenter.ListTaskPresenter;
import org.smartregister.reveal.util.AlertDialogUtils;
import org.smartregister.reveal.util.Constants.Action;
import org.smartregister.reveal.util.Constants.TaskRegister;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.util.RevealMapHelper;

import io.ona.kujaku.callbacks.OnLocationComponentInitializedCallback;
import io.ona.kujaku.layers.BoundaryLayer;
import io.ona.kujaku.utils.Constants;

import static org.smartregister.reveal.util.Constants.ANIMATE_TO_LOCATION_DURATION;
import static org.smartregister.reveal.util.Constants.CONFIGURATION.UPDATE_LOCATION_BUFFER_RADIUS;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.JSON_FORM_PARAM_JSON;
import static org.smartregister.reveal.util.Constants.Map;
import static org.smartregister.reveal.util.Constants.REQUEST_CODE_GET_JSON;
import static org.smartregister.reveal.util.Constants.VERTICAL_OFFSET;
import static org.smartregister.reveal.util.FamilyConstants.Intent.START_REGISTRATION;
import static org.smartregister.reveal.util.Utils.getGlobalConfig;

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


    private CardView sprayCardView;
    private TextView tvSprayStatus;
    private TextView tvPropertyType;
    private TextView tvSprayDate;
    private TextView tvSprayOperator;
    private TextView tvFamilyHead;
    private TextView tvReason;

    private CardView mosquitoCollectionCardView;
    private TextView tvMosquitoCollectionStatus;
    private TextView tvMosquitoTrapSetDate;
    private TextView tvMosquitoTrapFollowUpDate;

    private RefreshGeowidgetReceiver refreshGeowidgetReceiver = new RefreshGeowidgetReceiver();

    private boolean hasRequestedLocation;

    private Snackbar syncProgressSnackbar;


    private BaseDrawerContract.View drawerView;

    private RevealJsonFormUtils jsonFormUtils;

    private BoundaryLayer boundaryLayer;

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

        initializeCardViews();

        syncProgressSnackbar = Snackbar.make(rootView, getString(org.smartregister.R.string.syncing), Snackbar.LENGTH_INDEFINITE);

        jsonFormUtils = new RevealJsonFormUtils();
    }

    private void initializeCardViews() {
        sprayCardView = findViewById(R.id.spray_card_view);
        sprayCardView.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(View v, MotionEvent event) {
                //intercept clicks and interaction of map below card view
                return true;
            }
        });

        mosquitoCollectionCardView = findViewById(R.id.mosquito_collection_card_view);

        findViewById(R.id.btn_add_structure).setOnClickListener(this);

        findViewById(R.id.btn_collapse_spray_card_view).setOnClickListener(this);

        tvSprayStatus = findViewById(R.id.spray_status);
        tvPropertyType = findViewById(R.id.property_type);
        tvSprayDate = findViewById(R.id.spray_date);
        tvSprayOperator = findViewById(R.id.user_id);
        tvFamilyHead = findViewById(R.id.family_head);
        tvReason = findViewById(R.id.reason);

        tvMosquitoCollectionStatus = findViewById(R.id.trap_collection_status);
        tvMosquitoTrapSetDate = findViewById(R.id.trap_set_date);
        tvMosquitoTrapFollowUpDate = findViewById(R.id.trap_follow_up_date);

        findViewById(R.id.change_spray_status).setOnClickListener(this);

        findViewById(R.id.register_family).setOnClickListener(this);


        findViewById(R.id.task_register).setOnClickListener(this);

        findViewById(R.id.btn_collapse_mosquito_collection_card_view).setOnClickListener(this);

        findViewById(R.id.btn_record_mosquito_collection).setOnClickListener(this);

    }

    @Override
    public void closeCardView(int id) {
        if (id == R.id.btn_collapse_spray_card_view) {
            setViewVisibility(sprayCardView, false);
        } else if (id == R.id.btn_collapse_mosquito_collection_card_view) {
            setViewVisibility(mosquitoCollectionCardView, false);
        }
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
            public void onMapReady(@NonNull MapboxMap mapboxMap) {
                mapboxMap.setStyle(getString(R.string.reveal_satellite_style), new Style.OnStyleLoaded() {
                    @Override
                    public void onStyleLoaded(@NonNull Style style) {
                        geoJsonSource = style.getSourceAs(getString(R.string.reveal_datasource_name));

                        selectedGeoJsonSource = style.getSourceAs(getString(R.string.selected_datasource_name));
                        RevealMapHelper.addSymbolLayers(style, ListTasksActivity.this);
                    }
                });
                mMapboxMap = mapboxMap;


                mapboxMap.setMinZoomPreference(10);
                mapboxMap.setMaxZoomPreference(21);

                CameraPosition cameraPosition = new CameraPosition.Builder()
                        .zoom(16)
                        .build();
                mapboxMap.setCameraPosition(cameraPosition);


                listTaskPresenter.onMapReady();

                mapboxMap.addOnMapClickListener(new MapboxMap.OnMapClickListener() {
                    @Override
                    public boolean onMapClick(@NonNull LatLng point) {
                        listTaskPresenter.onMapClicked(mapboxMap, point);
                        return false;
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
            listTaskPresenter.onChangeInterventionStatus(IRS);
        } else if (v.getId() == R.id.btn_record_mosquito_collection) {
            listTaskPresenter.onChangeInterventionStatus(MOSQUITO_COLLECTION);
        } else if (v.getId() == R.id.btn_collapse_spray_card_view) {
            setViewVisibility(tvReason, false);
            closeCardView(v.getId());
        } else if (v.getId() == R.id.register_family) {
            registerFamily();
        } else if (v.getId() == R.id.task_register) {
            openTaskRegister();
        } else if (v.getId() == R.id.drawerMenu) {
            drawerView.openDrawerLayout();
        } else if (v.getId() == R.id.btn_collapse_mosquito_collection_card_view) {
            closeCardView(v.getId());
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
    public void setGeoJsonSource(@NonNull FeatureCollection featureCollection, Feature operationalArea) {
        if (geoJsonSource != null) {
            geoJsonSource.setGeoJson(featureCollection);
            if (operationalArea != null) {
                CameraPosition cameraPosition = mMapboxMap.getCameraForGeometry(operationalArea.geometry());
                if (cameraPosition != null) {
                    mMapboxMap.setCameraPosition(cameraPosition);
                }
                if (boundaryLayer == null) {
                    boundaryLayer = createBoundaryLayer(operationalArea);
                    kujakuMapView.addLayer(boundaryLayer);

                } else {
                    boundaryLayer.updateFeatures(FeatureCollection.fromFeature(operationalArea));
                }
            }
        }
    }

    private BoundaryLayer createBoundaryLayer(Feature operationalArea) {
        return new BoundaryLayer.Builder(FeatureCollection.fromFeature(operationalArea))
                .setLabelProperty(Map.NAME_PROPERTY)
                .setLabelTextSize(getResources().getDimension(R.dimen.operational_area_boundary_text_size))
                .setLabelColorInt(Color.WHITE)
                .setBoundaryColor(Color.WHITE)
                .setBoundaryWidth(getResources().getDimension(R.dimen.operational_area_boundary_width)).build();
    }

    @Override
    public void displayNotification(int title, int message, Object... formatArgs) {
        AlertDialogUtils.displayNotification(this, title, message, formatArgs);
    }

    @Override
    public void displayNotification(String message) {
        AlertDialogUtils.displayNotification(this, message);
    }

    @Override
    public void openCardView(CardDetails cardDetails) {
        if (cardDetails instanceof SprayCardDetails) {
            populateSprayCardTextViews((SprayCardDetails) cardDetails);
            sprayCardView.setVisibility(View.VISIBLE);
        } else if (cardDetails instanceof MosquitoCollectionCardDetails) {
            populateMosquitoCollectionCardTextViews((MosquitoCollectionCardDetails) cardDetails);
            mosquitoCollectionCardView.setVisibility(View.VISIBLE);
        }
    }

    private void populateSprayCardTextViews(SprayCardDetails sprayCardDetails) {
        tvSprayStatus.setTextColor(getResources().getColor(sprayCardDetails.getStatusColor()));
        tvSprayStatus.setText(sprayCardDetails.getStatusMessage());
        tvPropertyType.setText(sprayCardDetails.getPropertyType());
        tvSprayDate.setText(sprayCardDetails.getSprayDate());
        tvSprayOperator.setText(sprayCardDetails.getSprayOperator());
        tvFamilyHead.setText(sprayCardDetails.getFamilyHead());
        if (!TextUtils.isEmpty(sprayCardDetails.getReason())) {
            tvReason.setVisibility(View.VISIBLE);
            tvReason.setText(sprayCardDetails.getReason());
        } else {
            tvReason.setVisibility(View.GONE);
        }
    }

    private void populateMosquitoCollectionCardTextViews(MosquitoCollectionCardDetails mosquitoCollectionCardDetails) {
        tvMosquitoCollectionStatus.setText(mosquitoCollectionCardDetails.getStatus());
        tvMosquitoTrapSetDate.setText(getResources().getString(R.string.mosquito_collection_trap_set_date) + mosquitoCollectionCardDetails.getTrapSetDate());
        tvMosquitoTrapFollowUpDate.setText(getResources().getString(R.string.mosquito_collection_trap_follow_up_date) + mosquitoCollectionCardDetails.getTrapFollowUpDate());
    }

    @Override
    public void startJsonForm(JSONObject form) {
        jsonFormUtils.startJsonForm(form, this);
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
        IntentFilter filter = new IntentFilter(Action.STRUCTURE_TASK_SYNCED);
        LocalBroadcastManager.getInstance(getApplicationContext()).registerReceiver(refreshGeowidgetReceiver, filter);
        drawerView.onResume();
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

    @Override
    public RevealJsonFormUtils getJsonFormUtils() {
        return jsonFormUtils;
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
