package org.smartregister.reveal.interactor;

import com.vijay.jsonwizard.interactors.JsonFormInteractor;

import org.smartregister.reveal.widget.GeoWidgetFactory;

/**
 * Created by samuelgithengi on 12/13/18.
 */
public class RevealJsonFormInteractor extends JsonFormInteractor {


    private static final RevealJsonFormInteractor INSTANCE = new RevealJsonFormInteractor();

    private static final String GEOWIDGET = "geowidget";

    public static JsonFormInteractor getInstance() {
        return INSTANCE;
    }

    @Override
    protected void registerWidgets() {
        super.registerWidgets();
        map.put(GEOWIDGET, new GeoWidgetFactory());

    }

}
