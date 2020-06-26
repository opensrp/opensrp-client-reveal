package org.smartregister.reveal.layer;

import androidx.annotation.NonNull;

import com.mapbox.mapboxsdk.style.layers.Layer;
import com.mapbox.mapboxsdk.style.layers.RasterLayer;
import com.mapbox.mapboxsdk.style.sources.RasterSource;
import com.mapbox.mapboxsdk.style.sources.Source;
import com.mapbox.mapboxsdk.style.sources.TileSet;

import org.smartregister.reveal.BuildConfig;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import io.ona.kujaku.plugin.switcher.layer.BaseLayer;

/**
 * Created by samuelgithengi on 10/1/19.
 */
public class DigitalGlobeLayer extends BaseLayer {
    private String satelliteLayerId = "DG-EarthWatch-Satellite";
    private String satelliteSourceId = "dg-earthWatch-imagery";

    private LinkedHashSet<Layer> layers = new LinkedHashSet<>();
    private List<Source> sources = new ArrayList<>();

    public DigitalGlobeLayer() {
        createLayersAndSources();
    }

    private void createLayersAndSources() {
        String dgTileUrl = "https://access.maxar.com/earthservice/tmsaccess/tms/1.0.0/DigitalGlobe:ImageryTileService@EPSG:3857@png/{z}/{x}/{y}.png?connectId=" + BuildConfig.DG_CONNECT_ID;
        TileSet tileSet = new TileSet("1.0.0", dgTileUrl);
        tileSet.setScheme("tms");
        RasterSource rasterSource = new RasterSource(satelliteSourceId, tileSet, 256);

        RasterLayer rasterLayer = new RasterLayer(satelliteLayerId, satelliteSourceId);
        rasterLayer.setSourceLayer(satelliteSourceId);

        layers.add(rasterLayer);
        sources.add(rasterSource);
    }

    @NonNull
    @Override
    public String getDisplayName() {
        return "Digital Globe";
    }

    @NonNull
    @Override
    public String[] getSourceIds() {
        return new String[]{satelliteSourceId};
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
        return "dg-satellite-base-layer";
    }

    @NonNull
    @Override
    public String[] getLayerIds() {
        return new String[]{satelliteLayerId};
    }
}
