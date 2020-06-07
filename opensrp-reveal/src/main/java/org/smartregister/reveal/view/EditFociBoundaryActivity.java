package org.smartregister.reveal.view;

import android.content.Context;
import android.graphics.Color;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;

import com.google.gson.JsonArray;
import com.mapbox.android.core.permissions.PermissionsManager;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.geojson.MultiPolygon;
import com.mapbox.geojson.Polygon;
import com.mapbox.mapboxsdk.camera.CameraPosition;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.location.LocationComponent;
import com.mapbox.mapboxsdk.location.modes.RenderMode;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.OnMapReadyCallback;
import com.mapbox.mapboxsdk.maps.Style;
import com.mapbox.mapboxsdk.plugins.annotation.Circle;
import com.mapbox.mapboxsdk.style.sources.GeoJsonSource;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.domain.Location;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.EditFociboundaryContract;
import org.smartregister.reveal.presenter.EditFociBoundaryPresenter;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.EditBoundaryState;
import org.smartregister.reveal.util.RevealMapHelper;
import org.smartregister.sync.helper.LocationServiceHelper;

import java.util.Collections;

import io.ona.kujaku.callbacks.OnLocationComponentInitializedCallback;
import io.ona.kujaku.layers.FillBoundaryLayer;
import io.ona.kujaku.layers.KujakuLayer;
import io.ona.kujaku.listeners.OnDrawingCircleClickListener;
import io.ona.kujaku.listeners.OnKujakuLayerLongClickListener;
import io.ona.kujaku.manager.DrawingManager;
import timber.log.Timber;

import static org.smartregister.reveal.util.Utils.getLocationBuffer;
import static org.smartregister.reveal.util.Utils.getPixelsPerDPI;

/**
 * Created by Richard Kareko on 5/13/20.
 */

public class EditFociBoundaryActivity extends BaseMapActivity implements EditFociboundaryContract.View, OnLocationComponentInitializedCallback {

    private static final String TAG = EditFociBoundaryActivity.class.getName();

    protected RevealMapView kujakuMapView;
    private DrawingManager drawingManager;

    private Button deleteBtn ;
    private Button savePointBtn;
    private Button saveBoundaryBtn;
    private Button cancelBtn;

    private RevealApplication revealApplication = RevealApplication.getInstance();
    boolean locationComponentActive = false;
    private FillBoundaryLayer boundaryLayer;
    private EditFociBoundaryPresenter presenter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        presenter = new EditFociBoundaryPresenter(this);

        setContentView(R.layout.activity_edit_foci_boundary_map_view);
        kujakuMapView = findViewById(R.id.kmv_drawingBoundaries_mapView);
        kujakuMapView.onCreate(savedInstanceState);

        kujakuMapView.setDisableMyLocationOnMapMove(true);
        kujakuMapView.getMapboxLocationComponentWrapper().setOnLocationComponentInitializedCallback(this);

        if (revealApplication.getOperationalArea() != null) {
            FillBoundaryLayer.Builder boundaryBuilder = new FillBoundaryLayer.Builder(FeatureCollection.fromFeature(revealApplication.getOperationalArea()))
                    .setLabelProperty(Constants.Map.NAME_PROPERTY)
                    .setLabelTextSize(getResources().getDimension(R.dimen.operational_area_boundary_text_size))
                    .setLabelColorInt(Color.WHITE)
                    .setBoundaryColor(Color.WHITE)
                    .setBoundaryWidth(getResources().getDimension(R.dimen.operational_area_boundary_width));
            boundaryLayer = boundaryBuilder.build();
        }


        this.deleteBtn = findViewById(R.id.btn_drawingBoundaries_delete);
        this.deleteBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                presenter.onDeletePoint(view);
            }
        });

        this.savePointBtn = findViewById(R.id.btn_drawingBoundaries_save_point);
        this.savePointBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (drawingManager != null) {
                    if (!drawingManager.isDrawingEnabled()) {
                    } else {
                        kujakuMapView.addLayer(boundaryLayer);
                        drawingManager.stopDrawingAndDisplayLayer();
                        toggleButtons(EditBoundaryState.FINISHED);
                    }
                } else {
                    Log.e(TAG, "Drawing manager instance is null");
                }

                deleteBtn.setEnabled(false);
            }
        });

        this.saveBoundaryBtn = findViewById(R.id.btn_drawingBoundaries_save);
        this.saveBoundaryBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                MultiPolygon updatedOA = MultiPolygon.fromPolygons(Collections.singletonList((Polygon) boundaryLayer.getFeatureCollection().features().get(0).geometry()));
                JSONObject updatedOAJson = null;
                JSONArray updatedCoords = null;
                try {
                    updatedOAJson = new JSONObject(updatedOA.toJson());
                    updatedCoords = (JSONArray) updatedOAJson.get("coordinates");
                } catch (JSONException e) {
                    Timber.e(e);
                }
                Location operationalAreaLocation = LocationServiceHelper.locationGson.fromJson(revealApplication.getOperationalArea().toJson(), Location.class);
                JsonArray updatedCoordsJsonArray = LocationServiceHelper.locationGson.fromJson(updatedCoords.toString(), JsonArray.class);
                operationalAreaLocation.getGeometry().setCoordinates(updatedCoordsJsonArray);
                presenter.onSaveEditedBoundary(operationalAreaLocation);
            }
        });

        cancelBtn = findViewById(R.id.btn_drawingBoundaries_cancel);
        this.cancelBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                presenter.onCancelEditBoundaryChanges();
            }
        });

        toggleButtons(EditBoundaryState.START);

        String featureCollection = revealApplication.getFeatureCollection().toJson();
        String finalFeatureCollection = featureCollection;
        com.mapbox.geojson.Feature finalOperationalAreaFeature = revealApplication.getOperationalArea();
        boolean finalLocationComponentActive = locationComponentActive;
        kujakuMapView.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(@NonNull MapboxMap mapboxMap) {
                kujakuMapView.focusOnUserLocation(true);
                Style.Builder builder = new Style.Builder().fromUri(getString(R.string.reveal_satellite_style));
                mapboxMap.setStyle(builder,  new Style.OnStyleLoaded() {
                    @Override
                    public void onStyleLoaded(@NonNull Style style) {

                        GeoJsonSource geoJsonSource = style.getSourceAs(getString(R.string.reveal_datasource_name));

                        if (geoJsonSource != null && StringUtils.isNotBlank(finalFeatureCollection)) {
                            geoJsonSource.setGeoJson(finalFeatureCollection);
                        }

                        RevealMapHelper.addCustomLayers(style, EditFociBoundaryActivity.this);

                        kujakuMapView.setMapboxMap(mapboxMap);

                        RevealMapHelper.addBaseLayers(kujakuMapView, style, EditFociBoundaryActivity.this);

                        drawingManager = new DrawingManager(kujakuMapView, mapboxMap, style);

                        drawingManager.addOnDrawingCircleClickListener(new OnDrawingCircleClickListener() {
                            @Override
                            public void onCircleClick(@NonNull Circle circle) {
                                Toast.makeText(EditFociBoundaryActivity.this,
                                        getString(R.string.circle_clicked), Toast.LENGTH_SHORT).show();
                                deleteBtn.setEnabled(drawingManager.getCurrentKujakuCircle() != null);
                                toggleButtons(EditBoundaryState.EDITTING);
                            }

                            @Override
                            public void onCircleNotClick(@NonNull LatLng latLng) {
                                Toast.makeText(EditFociBoundaryActivity.this,
                                        getString(R.string.circle_not_clicked), Toast.LENGTH_SHORT).show();
                                deleteBtn.setEnabled(false);
                            }
                        });

                        drawingManager.addOnKujakuLayerLongClickListener(new OnKujakuLayerLongClickListener() {
                            @Override
                            public void onKujakuLayerLongClick(@NonNull KujakuLayer kujakuLayer) {

                                if (drawingManager.isDrawingEnabled()) {
                                    savePointBtn.setText(R.string.save_point);
                                }
                            }
                        });

                        //jump straight into edit mode
                        enabledrawingMode(mapboxMap);
                    }
                }); //end of set style

                mapboxMap.getUiSettings().setRotateGesturesEnabled(false);

                kujakuMapView.setMapboxMap(mapboxMap);
                float bufferRadius = getLocationBuffer() / getPixelsPerDPI(getResources());
                kujakuMapView.setLocationBufferRadius(bufferRadius);


                if (finalOperationalAreaFeature != null && !finalLocationComponentActive) {
                    CameraPosition cameraPosition = mapboxMap.getCameraForGeometry(finalOperationalAreaFeature.geometry());
                    if (cameraPosition != null) {
                        mapboxMap.setCameraPosition(cameraPosition);
                    }
                } else {
                    kujakuMapView.focusOnUserLocation(true, bufferRadius, RenderMode.COMPASS);
                }

            }
        });
    }

    private void enabledrawingMode(MapboxMap mapboxMap) {
        boundaryLayer.disableLayerOnMap(mapboxMap);
        if (drawingManager != null) {
            if (!drawingManager.isDrawingEnabled()) {
                if (drawingManager.startDrawing( boundaryLayer)) {
                    savePointBtn.setText(R.string.save_point);
                }
            } else {
                drawingManager.stopDrawingAndDisplayLayer();
            }
        } else {
            Log.e(TAG, "Drawing manager instance is null");
        }

        deleteBtn.setEnabled(false);
    }

    @Override
    public void toggleButtons(EditBoundaryState state) {
        switch (state) {
            case EDITTING:
                cancelBtn.setVisibility(View.GONE);
                saveBoundaryBtn.setVisibility(View.GONE);
                deleteBtn.setVisibility(View.VISIBLE);
                savePointBtn.setVisibility(View.VISIBLE);
                break;
            case START:
            case FINISHED:
            default:
                cancelBtn.setVisibility(View.VISIBLE);
                saveBoundaryBtn.setVisibility(View.VISIBLE);
                deleteBtn.setVisibility(View.GONE);
                savePointBtn.setVisibility(View.GONE);
                break;
        }
    }

    @Override
    public void exitEditBoundaryActivity() {
        if (drawingManager != null) {
            if (drawingManager.isDrawingEnabled()) {
                drawingManager.stopDrawingAndDisplayLayer();
            }
        }
        finish();
    }

    @Override
    public Context getContext() {
        return this;
    }

    @Override
    public void deletePoint(View view) {
        if (drawingManager != null) {
            drawingManager.deleteDrawingCurrentCircle();
            view.setEnabled(false);
        }
    }

    @Override
    public void onLocationComponentInitialized() {
        if (PermissionsManager.areLocationPermissionsGranted(this)) {
            LocationComponent locationComponent = kujakuMapView.getMapboxLocationComponentWrapper()
                    .getLocationComponent();
            locationComponent.applyStyle(getApplicationContext(), R.style.LocationComponentStyling);
        }
    }
}
