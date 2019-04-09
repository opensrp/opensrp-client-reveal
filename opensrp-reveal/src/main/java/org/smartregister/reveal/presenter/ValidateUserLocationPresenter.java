package org.smartregister.reveal.presenter;

import android.location.Location;
import android.os.SystemClock;
import android.util.Log;

import com.mapbox.mapboxsdk.geometry.LatLng;

import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.UserLocationContract;
import org.smartregister.reveal.contract.UserLocationContract.UserLocationCallback;
import org.smartregister.reveal.contract.UserLocationContract.UserLocationView;
import org.smartregister.reveal.util.AppExecutors;

import java.util.Timer;
import java.util.TimerTask;

/**
 * Created by samuelgithengi on 2/13/19.
 */
public class ValidateUserLocationPresenter implements UserLocationContract.UserLocationPresenter {

    private static final String TAG = "ValidateUserLocation";

    private UserLocationView locationView;

    private UserLocationCallback callback;

    private long resolutionStarted;

    private AppExecutors appExecutors;

    protected ValidateUserLocationPresenter(UserLocationView locationView, UserLocationCallback callback) {
        this.locationView = locationView;
        this.callback = callback;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
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
        resolutionStarted = SystemClock.elapsedRealtime();
        waitForUserLocation(0);
    }


    private void waitForUserLocation(long elapsedTimeInMillis) {
        Location location = locationView.getUserCurrentLocation();
        Log.d(TAG, "user location: " + location);
        if (location == null) {
            if (elapsedTimeInMillis / 1000 >= BuildConfig.RESOLVE_LOCATION_TIMEOUT_IN_SECONDS) {
                appExecutors.mainThread().execute(() -> onGetUserLocationFailed());
            } else {
                if (elapsedTimeInMillis == 0) {//first try running in main thread ; show progress dialog.
                    locationView.showProgressDialog(R.string.narrowing_location_title, R.string.narrowing_location_message);
                }
                new Timer().schedule(new TimerTask() {
                    @Override
                    public void run() {
                        waitForUserLocation(SystemClock.elapsedRealtime() - resolutionStarted);
                    }
                }, 2000);
            }
        } else {
            appExecutors.mainThread().execute(() -> onGetUserLocation(location));

        }
    }


}
