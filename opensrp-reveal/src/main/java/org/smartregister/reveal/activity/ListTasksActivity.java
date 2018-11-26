package org.smartregister.reveal.activity;

import android.os.Bundle;

import com.mapbox.geojson.FeatureCollection;
import com.mapbox.mapboxsdk.camera.CameraPosition;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.OnMapReadyCallback;
import com.mapbox.mapboxsdk.style.sources.GeoJsonSource;

import org.smartregister.reveal.R;
import org.smartregister.util.Utils;

/**
 * Created by samuelgithengi on 11/20/18.
 */
public class ListTasksActivity extends BaseMapActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_list_tasks);
        kujakuMapView = findViewById(R.id.kujakuMapView);
        kujakuMapView.onCreate(savedInstanceState);

        kujakuMapView.setStyleUrl("asset://reveal-streets-style.json");

        kujakuMapView.showCurrentLocationBtn(true);
        kujakuMapView.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(MapboxMap mapboxMap) {

                mapboxMap.setMinZoomPreference(14);
                mapboxMap.setMaxZoomPreference(21);

                String geoJson = Utils.readAssetContents(ListTasksActivity.this, "geojson.json");

                FeatureCollection featureCollection = FeatureCollection.fromJson(geoJson);

                GeoJsonSource geoJsonSource = mapboxMap.getSourceAs("reveal-data-set");

                if (geoJsonSource != null) {
                    geoJsonSource.setGeoJson(featureCollection);
                }

                CameraPosition cameraPosition = new CameraPosition.Builder()
                        .target(new LatLng(-14.1706623, 32.5987837))
                        .zoom(18)
                        .build();
                mapboxMap.setCameraPosition(cameraPosition);

            }
        });

    }
}
