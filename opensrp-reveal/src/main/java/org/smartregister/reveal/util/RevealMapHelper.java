package org.smartregister.reveal.util;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;

import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.style.expressions.Expression;
import com.mapbox.mapboxsdk.style.layers.SymbolLayer;

import org.smartregister.reveal.R;
import org.smartregister.reveal.util.Constants.StructureType;

import static com.mapbox.mapboxsdk.style.expressions.Expression.eq;
import static com.mapbox.mapboxsdk.style.expressions.Expression.get;
import static com.mapbox.mapboxsdk.style.expressions.Expression.interpolate;
import static com.mapbox.mapboxsdk.style.expressions.Expression.linear;
import static com.mapbox.mapboxsdk.style.expressions.Expression.literal;
import static com.mapbox.mapboxsdk.style.expressions.Expression.zoom;
import static com.mapbox.mapboxsdk.style.layers.PropertyFactory.iconImage;
import static com.mapbox.mapboxsdk.style.layers.PropertyFactory.iconSize;
import static org.smartregister.reveal.util.Constants.GeoJSON.TYPE;

/**
 * Created by samuelgithengi on 2/20/19.
 */
public class RevealMapHelper {


    private static final String LARVAL_BREEDING_ICON = "larval-breeding-icon";

    private static final String MOSQUITO_COLLECTION_ICON = "mosquito-collection-icon";


    private static final String LARVAL_BREEDING_LAYER = "larval-breeding-layer";

    private static final String MOSQUITO_COLLECTION_LAYER = "mosquito-collection-layer";


    public static void addSymbolLayers(MapboxMap mMapboxMap, Context context) {
        Expression dynamicIconSize = interpolate(linear(), zoom(),
                literal(13.98f), literal(0),
                literal(17.79f), literal(1.5f),
                literal(18.8f), literal(2));

        Bitmap icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_mosquito);
        mMapboxMap.addImage(MOSQUITO_COLLECTION_ICON, icon);
        SymbolLayer symbolLayer = new SymbolLayer(MOSQUITO_COLLECTION_LAYER, context.getString(R.string.reveal_datasource_name));
        symbolLayer.setProperties(
                iconImage(MOSQUITO_COLLECTION_ICON),
                iconSize(dynamicIconSize));
        symbolLayer.setFilter(eq(get(TYPE), StructureType.MOSQUITO_COLLECTION_POINT));
        mMapboxMap.addLayer(symbolLayer);


        icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_bleeding);
        mMapboxMap.addImage(LARVAL_BREEDING_ICON, icon);
        symbolLayer = new SymbolLayer(LARVAL_BREEDING_LAYER, context.getString(R.string.reveal_datasource_name));
        symbolLayer.setProperties(
                iconImage(LARVAL_BREEDING_ICON),
                iconSize(dynamicIconSize));

        symbolLayer.setFilter(eq(get(TYPE), StructureType.LARVAL_BREEDING_SITE));

        mMapboxMap.addLayer(symbolLayer);
    }
}
