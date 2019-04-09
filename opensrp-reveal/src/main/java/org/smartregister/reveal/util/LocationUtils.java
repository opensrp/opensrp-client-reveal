package org.smartregister.reveal.util;

import android.content.Context;
import android.location.Location;

import io.ona.kujaku.interfaces.ILocationClient;
import io.ona.kujaku.listeners.BaseLocationListener;
import io.ona.kujaku.location.clients.GoogleLocationClient;

/**
 * Created by samuelgithengi on 3/20/19.
 */
public class LocationUtils {

    private ILocationClient locationClient;

    public LocationUtils(Context context) {
        locationClient = new GoogleLocationClient(context);
    }

    public void requestLocationUpdates(BaseLocationListener locationListener) {
        locationClient.requestLocationUpdates(locationListener);
    }

    public Location getLastLocation() {
        return locationClient.getLastLocation();
    }

    public void stopLocationClient() {
        locationClient.close();
        locationClient = null;
    }


}
