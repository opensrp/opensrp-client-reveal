package org.smartregister.reveal.presenter;

import android.app.ProgressDialog;
import android.content.Intent;
import android.location.Location;
import android.os.Handler;
import android.os.Looper;
import android.support.annotation.StringRes;
import android.support.v7.app.AlertDialog;
import android.util.Log;
import android.view.View;
import android.widget.LinearLayout;

import com.mapbox.mapboxsdk.geometry.LatLng;
import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interactors.JsonFormInteractor;
import com.vijay.jsonwizard.presenters.JsonFormFragmentPresenter;
import com.vijay.jsonwizard.utils.ValidationStatus;

import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.activity.RevealJsonForm;
import org.smartregister.reveal.contract.PasswordRequestCallback;
import org.smartregister.reveal.util.PasswordDialogUtils;
import org.smartregister.reveal.view.RevealMapView;
import org.smartregister.reveal.widget.GeoWidgetFactory;

/**
 * Created by samuelgithengi on 1/30/19.
 */
public class RevealJsonFormFragmentPresenter extends JsonFormFragmentPresenter implements PasswordRequestCallback {

    private JsonFormFragment formFragment;

    private AlertDialog passwordDialog;

    private RevealJsonForm revealJsonForm;

    private RevealMapView mapView;

    private ProgressDialog progressDialog;

    public RevealJsonFormFragmentPresenter(JsonFormFragment formFragment, JsonFormInteractor jsonFormInteractor) {
        super(formFragment, jsonFormInteractor);
        this.formFragment = formFragment;
        passwordDialog = PasswordDialogUtils.initPasswordDialog(formFragment.getActivity(), this);
        revealJsonForm = (RevealJsonForm) formFragment.getActivity();
        progressDialog = new ProgressDialog(revealJsonForm);
        progressDialog.setCancelable(false);

    }

    @Override
    public ValidationStatus writeValuesAndValidate(LinearLayout mainView) {
        ValidationStatus validationStatus = super.writeValuesAndValidate(mainView);
        if (validationStatus.isValid()) {
            for (View childAt : formFragment.getJsonApi().getFormDataViews()) {
                if (childAt instanceof RevealMapView) {
                    RevealMapView mapView = (RevealMapView) childAt;
                    validationStatus = GeoWidgetFactory.validate(formFragment, mapView);
                    if (validationStatus.isValid()) {
                        if (BuildConfig.VALIDATE_FAR_STRUCTURES) {
                            validateUserLocation(mapView);
                        } else {
                            onValidateUserLocation(true);
                        }
                    }
                }
            }

        }
        return validationStatus;
    }


    private void validateUserLocation(RevealMapView mapView) {
        this.mapView = mapView;
        Location location = mapView.getLocationClient() == null ? null : mapView.getLocationClient().getLastLocation();
        if (location != null) {
            onGetUserLocation(location);
        } else {
            requestUserLocation();
        }

    }

    private void requestUserLocation() {
        mapView.setWarmGps(true, revealJsonForm.getString(R.string.location_service_disabled), revealJsonForm.getString(R.string.enable_location_services_rejected));
        revealJsonForm.setRequestedLocation(true);
    }

    private void onGetUserLocation(Location location) {
        LatLng structureToAdd = mapView.getMapboxMap().getCameraPosition().target;
        double offset = structureToAdd.distanceTo(new LatLng(location.getLatitude(), location.getLongitude()));
        if (offset > BuildConfig.MY_LOCATION_BUFFER) {
            onValidateUserLocation(false);
        } else {
            onValidateUserLocation(true);
        }

    }

    private void onValidateUserLocation(boolean isValid) {
        hideProgressDialog();
        if (isValid) {
            Intent returnIntent = new Intent();
            getView().onFormFinish();
            returnIntent.putExtra("json", getView().getCurrentJsonState());
            returnIntent.putExtra(JsonFormConstants.SKIP_VALIDATION, Boolean.valueOf(formFragment.getMainView().getTag(com.vijay.jsonwizard.R.id.skip_validation).toString()));
            getView().finishWithResult(returnIntent);

        } else {
            requestUserPassword();
        }
    }

    @Override
    public void onSaveClick(LinearLayout mainView) {
        writeValuesAndValidate(mainView);
    }

    private void requestUserPassword() {
        if (passwordDialog != null) {
            passwordDialog.show();
        }
    }

    public void onGetUserLocationFailed() {
        onValidateUserLocation(false);
    }

    @Override
    public void onPasswordVerified() {
        onValidateUserLocation(true);
    }


    public void showProgressDialog(@StringRes int title, @StringRes int message) {
        if (progressDialog != null) {
            progressDialog.setTitle(title);
            progressDialog.setMessage(revealJsonForm.getString(message));
            progressDialog.show();
        }
    }


    public void hideProgressDialog() {
        if (progressDialog != null) {
            progressDialog.dismiss();
        }
    }


    public void waitForUserLocation() {
        Location location = mapView.getLocationClient() == null ? null : mapView.getLocationClient().getLastLocation();
        Log.d(RevealJsonFormFragmentPresenter.class.getName(), "user location: " + location);
        if (location == null) {
            showProgressDialog(R.string.narrowing_location_title, R.string.narrowing_location_message);
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
