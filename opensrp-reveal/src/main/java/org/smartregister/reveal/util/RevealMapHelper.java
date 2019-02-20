package org.smartregister.reveal.util;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;

import com.mapbox.mapboxsdk.maps.MapboxMap;
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

/**
 * Created by samuelgithengi on 2/20/19.
 */
public class RevealMapHelper {

    public static void addSymbolLayers(MapboxMap mMapboxMap, Context context) {
        Bitmap icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_mosquito);
        mMapboxMap.addImage("mosquito-collection-image", icon);
        SymbolLayer symbolLayer = new SymbolLayer("mosquito-collection-layer", context.getString(R.string.reveal_datasource_name));
        symbolLayer.setProperties(
                iconImage("mosquito-collection-image"),
                iconSize(interpolate(linear(), zoom(),
                        literal(13.98), literal(0),
                        literal(16), literal(1),
                        literal(18), literal(2)))
        );
        symbolLayer.setFilter(eq(get("type"), StructureType.MOSQUITO_COLLECTION_POINT));
        mMapboxMap.addLayer(symbolLayer);


        icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_larval);
        mMapboxMap.addImage("larval-breeding-image", icon);
        symbolLayer = new SymbolLayer("larval-breeding-layer", context.getString(R.string.reveal_datasource_name));
        symbolLayer.setProperties(
                iconImage("larval-breeding-image"),
                iconSize(interpolate(linear(), zoom(),
                        literal(13.98), literal(0),
                        literal(16), literal(1),
                        literal(18), literal(2)))
        );

        symbolLayer.setFilter(eq(get("type"), StructureType.LARVAL_BREEDING_SITE));

        mMapboxMap.addLayer(symbolLayer);
    }
}
