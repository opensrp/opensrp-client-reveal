package org.smartregister.reveal.presenter;

import android.location.Location;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;

import com.mapbox.mapboxsdk.geometry.LatLng;

import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.UserLocationContract;
import org.smartregister.reveal.contract.UserLocationContract.UserLocationCallback;
import org.smartregister.reveal.contract.UserLocationContract.UserLocationView;

/**
 * Created by samuelgithengi on 2/13/19.
 */
public class ValidateUserLocationPresenter implements UserLocationContract.UserLocationPresenter {

    private static final String TAG = "ValidateUserLocation";

    private UserLocationView locationView;

    private UserLocationCallback callback;

    public ValidateUserLocationPresenter(UserLocationView locationView, UserLocationCallback callback) {
        this.locationView = locationView;
        this.callback = callback;
    }

    @Override
    public void requestUserLocation() {
        locationView.requestUserLocation();
    }

    @Override
    public void onGetUserLocation(Location location) {
        locationView.hideProgressDialog();
        double offset = callback.getTargetCoordinates().distanceTo(
                new LatLng(location.getLatitude(), location.getLongitude()));
        if (offset > BuildConfig.MY_LOCATION_BUFFER) {
            callback.requestUserPassword();
        } else {
            callback.onLocationValidated();
        }
    }

    @Override
    public void onGetUserLocationFailed() {
        locationView.hideProgressDialog();
        callback.requestUserPassword();
    }

    @Override
    public void waitForUserLocation() {
        Location location = locationView.getUserCurrentLocation();
        Log.d(TAG, "user location: " + location);
        if (location == null) {
            locationView.showProgressDialog(R.string.narrowing_location_title, R.string.narrowing_location_message);
            new Handler(Looper.getMainLooper()).postDelayed(new Runnable() {
                @Override
                public void run() {
                    waitForUserLocation();
                }
            }, 2000);
        } else {
            onGetUserLocation(location);
        }
    }

}
