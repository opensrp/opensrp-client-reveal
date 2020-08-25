package org.smartregister.reveal.layer;

import androidx.annotation.NonNull;

import com.mapbox.mapboxsdk.style.layers.Layer;
import com.mapbox.mapboxsdk.style.layers.RasterLayer;
import com.mapbox.mapboxsdk.style.sources.RasterSource;
import com.mapbox.mapboxsdk.style.sources.Source;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import io.ona.kujaku.plugin.switcher.layer.BaseLayer;

/**
 * Created by samuelgithengi on 10/1/19.
 */
public class MapBoxLayer extends BaseLayer {
    private String satelliteLayerId = "mapbox-satellite";
    private String satelliteSourceId = "mapbox://mapbox.satellite";

    private LinkedHashSet<Layer> layers = new LinkedHashSet<>();
    private List<Source> sources = new ArrayList<>();

    public MapBoxLayer() {
        createLayersAndSources();
    }

    private void createLayersAndSources() {
        RasterSource rasterSource = new RasterSource(satelliteSourceId, "mapbox://mapbox.satellite", 256);

        RasterLayer rasterLayer = new RasterLayer(satelliteLayerId, satelliteSourceId);
        rasterLayer.setSourceLayer("mapbox-satellite");

        layers.add(rasterLayer);
        sources.add(rasterSource);
    }

    @NonNull
    @Override
    public String getDisplayName() {
        return "Mapbox Satellite";
    }

    @NonNull
    @Override
    public String[] getSourceIds() {
        return new String[] {satelliteSourceId};
    }

    @Override
    public LinkedHashSet<Layer> getLayers() {
        return layers;
    }

    @Override
    public List<Source> getSources() {
        return sources;
    }

    @NonNull
    @Override
    public String getId() {
        return "mapbox-satellite-base-layer";
    }

    @NonNull
    @Override
    public String[] getLayerIds() {
        return new String[] {satelliteLayerId};
    }
}
