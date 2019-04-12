package org.smartregister.reveal.util;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.support.annotation.NonNull;

import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.Style;
import com.mapbox.mapboxsdk.style.expressions.Expression;
import com.mapbox.mapboxsdk.style.layers.CircleLayer;
import com.mapbox.mapboxsdk.style.layers.SymbolLayer;

import org.smartregister.reveal.R;
import org.smartregister.reveal.util.Constants.StructureType;

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
import static org.smartregister.reveal.util.Constants.GeoJSON.IS_INDEX_CASE;
import static org.smartregister.reveal.util.Constants.GeoJSON.TYPE;

/**
 * Created by samuelgithengi on 2/20/19.
 */
public class RevealMapHelper {


    private static final String LARVAL_BREEDING_ICON = "larval-breeding-icon";

    private static final String MOSQUITO_COLLECTION_ICON = "mosquito-collection-icon";

    private static final String INDEX_CASE_TARGET_ICON =    "index-case-target-icon";


    private static final String LARVAL_BREEDING_LAYER = "larval-breeding-layer";

    private static final String MOSQUITO_COLLECTION_LAYER = "mosquito-collection-layer";

    private static final String INDEX_CASE_SYMBOL_LAYER = "index-case-symbol-layer";

    private static final String INDEX_CASE_CIRCLE_LAYER = "index-case-circle-layer";


    public static void addCustomLayers(@NonNull Style mMapboxMapStyle, Context context) {
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
        symbolLayer.setFilter(eq(get("uid"), "ff6368d3-d62d-49bc-9936-26eb2e1433e6"));
        mMapboxMapStyle.addLayer(symbolLayer);

        // index case circle layer
        CircleLayer circleLayer = new CircleLayer(INDEX_CASE_CIRCLE_LAYER, context.getString(R.string.reveal_datasource_name));
        circleLayer.withProperties(
                circleOpacity(0f),
                circleColor(Color.parseColor("#ffffff")),
                circleRadius(200f),
                circleStrokeColor("#ffffff"),
                circleStrokeWidth(2f),
                circleStrokeOpacity(1f)
        );
//        circleLayer.setFilter(eq(get(IS_INDEX_CASE), true));
        circleLayer.setFilter(eq(get("uid"), "ff6368d3-d62d-49bc-9936-26eb2e1433e6"));
        mMapboxMapStyle.addLayer(circleLayer);
    }
}
