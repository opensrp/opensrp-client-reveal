package org.smartregister.reveal.util;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.location.Location;
import android.support.annotation.NonNull;
import android.util.Log;

import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.geojson.Feature;
import com.mapbox.geojson.Geometry;
import com.mapbox.mapboxsdk.maps.Style;
import com.mapbox.mapboxsdk.style.expressions.Expression;
import com.mapbox.mapboxsdk.style.layers.CircleLayer;
import com.mapbox.mapboxsdk.style.layers.SymbolLayer;
import com.mapbox.mapboxsdk.style.sources.GeoJsonSource;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.reveal.R;
import org.smartregister.reveal.repository.RevealMappingHelper;
import org.smartregister.reveal.util.Constants.StructureType;

import java.util.List;

import static com.mapbox.mapboxsdk.style.expressions.Expression.eq;
import static com.mapbox.mapboxsdk.style.expressions.Expression.get;
import static com.mapbox.mapboxsdk.style.expressions.Expression.interpolate;
import static com.mapbox.mapboxsdk.style.expressions.Expression.linear;
import static com.mapbox.mapboxsdk.style.expressions.Expression.literal;
import static com.mapbox.mapboxsdk.style.expressions.Expression.zoom;
import static com.mapbox.mapboxsdk.style.layers.PropertyFactory.circleColor;
import static com.mapbox.mapboxsdk.style.layers.PropertyFactory.circleOpacity;
import static com.mapbox.mapboxsdk.style.layers.PropertyFactory.circleRadius;
import static com.mapbox.mapboxsdk.style.layers.PropertyFactory.circleStrokeColor;
import static com.mapbox.mapboxsdk.style.layers.PropertyFactory.circleStrokeOpacity;
import static com.mapbox.mapboxsdk.style.layers.PropertyFactory.circleStrokeWidth;
import static com.mapbox.mapboxsdk.style.layers.PropertyFactory.iconImage;
import static com.mapbox.mapboxsdk.style.layers.PropertyFactory.iconSize;
import static org.smartregister.reveal.util.Constants.GeoJSON.TYPE;

/**
 * Created by samuelgithengi on 2/20/19.
 */
public class RevealMapHelper {

    private static final String TAG = RevealMapHelper.class.getName();

    private static final String LARVAL_BREEDING_ICON = "larval-breeding-icon";

    private static final String MOSQUITO_COLLECTION_ICON = "mosquito-collection-icon";

    private static final String INDEX_CASE_TARGET_ICON =    "index-case-target-icon";


    private static final String LARVAL_BREEDING_LAYER = "larval-breeding-layer";

    private static final String MOSQUITO_COLLECTION_LAYER = "mosquito-collection-layer";

    private static final String INDEX_CASE_SYMBOL_LAYER = "index-case-symbol-layer";

    private static final String INDEX_CASE_CIRCLE_LAYER = "index-case-circle-layer";

    private static final String INDEX_CASE_SOURCE = "index_case_source";


    public static void addCustomLayers(@NonNull  Style mMapboxMapStyle, Context context) {

        Expression dynamicIconSize = interpolate(linear(), zoom(),
                literal(13.98f), literal(0),
                literal(17.79f), literal(1.5f),
                literal(18.8f), literal(2));

        // mosquito collection symbol layer
        Bitmap icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_mosquito);
        mMapboxMapStyle.addImage(MOSQUITO_COLLECTION_ICON, icon);
        SymbolLayer symbolLayer = new SymbolLayer(MOSQUITO_COLLECTION_LAYER, context.getString(R.string.reveal_datasource_name));
        symbolLayer.setProperties(
                iconImage(MOSQUITO_COLLECTION_ICON),
                iconSize(dynamicIconSize));
        symbolLayer.setFilter(eq(get(TYPE), StructureType.MOSQUITO_COLLECTION_POINT));
        mMapboxMapStyle.addLayer(symbolLayer);

        // larval breeding symbol layer
        icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_breeding);
        mMapboxMapStyle.addImage(LARVAL_BREEDING_ICON, icon);
        symbolLayer = new SymbolLayer(LARVAL_BREEDING_LAYER, context.getString(R.string.reveal_datasource_name));
        symbolLayer.setProperties(
                iconImage(LARVAL_BREEDING_ICON),
                iconSize(dynamicIconSize));
        symbolLayer.setFilter(eq(get(TYPE), StructureType.LARVAL_BREEDING_SITE));
        mMapboxMapStyle.addLayer(symbolLayer);

        // index case symbol layer
        dynamicIconSize = interpolate(linear(), zoom(),
                literal(13.98f), literal(1),
                literal(17.79f), literal(3f),
                literal(18.8f), literal(4));
        icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_index_case_target_icon);
        mMapboxMapStyle.addImage(INDEX_CASE_TARGET_ICON, icon);
        symbolLayer = new SymbolLayer(INDEX_CASE_SYMBOL_LAYER, context.getString(R.string.reveal_datasource_name));
        symbolLayer.setProperties(
                iconImage(INDEX_CASE_TARGET_ICON),
                iconSize(dynamicIconSize));
//        symbolLayer.setFilter(eq(get(IS_INDEX_CASE), true));
        symbolLayer.setFilter(eq(get("uid"), "d2fb1ee0-5d30-11e9-8647-d663bd873d93"));
        mMapboxMapStyle.addLayer(symbolLayer);
    }

    public static void addFocusInvestigationBoundaryCircle(Style mMapboxMapStyle, Context context) {
        try {
            // index case circle layer
            GeoJsonSource indexCaseSource = new GeoJsonSource(INDEX_CASE_SOURCE);
            GeoJsonSource revealSource = mMapboxMapStyle.getSourceAs(context.getString(R.string.reveal_datasource_name));
            //        List<Feature> indexCase = revealSource.querySourceFeatures(eq(get(IS_INDEX_CASE), true));
            List<Feature> features = revealSource.querySourceFeatures(eq(get("uid"), "d2fb1ee0-5d30-11e9-8647-d663bd873d93"));
            if (features.size() > 0) {
                Feature indexCase = features.get(0);

                Location centre = (new RevealMappingHelper()).getCenter(indexCase.geometry().toJson());
                JSONObject feature = new JSONObject(indexCase.toJson());
                JSONObject geometry = new JSONObject();
                geometry.put("type", "Point");
                geometry.put("coordinates", new JSONArray(new Double[]{centre.getLongitude(), centre.getLatitude()}));
                feature.put("geometry", geometry);
                indexCaseSource.setGeoJson(Feature.fromJson(feature.toString()));

                CircleLayer circleLayer = new CircleLayer(INDEX_CASE_CIRCLE_LAYER, INDEX_CASE_SOURCE);
                circleLayer.withProperties(
                        circleOpacity(0f),
                        circleColor(Color.parseColor("#ffffff")),
                        circleRadius(200f),
                        circleStrokeColor("#ffffff"),
                        circleStrokeWidth(2f),
                        circleStrokeOpacity(1f)
                );
//        circleLayer.setFilter(eq(get(IS_INDEX_CASE), true));
                if (mMapboxMapStyle.getSource(INDEX_CASE_SOURCE) == null) {
                    mMapboxMapStyle.addSource(indexCaseSource);
                    mMapboxMapStyle.addLayer(circleLayer);
                }
            }
        } catch (JSONException e) {
            Log.e(TAG, e.getStackTrace().toString());
        }
    }
}
