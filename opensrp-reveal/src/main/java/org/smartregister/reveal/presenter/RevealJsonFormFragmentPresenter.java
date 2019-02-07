package org.smartregister.reveal.presenter;

import android.Manifest;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.support.v4.app.ActivityCompat;
import android.support.v7.app.AlertDialog;
import android.view.View;
import android.widget.LinearLayout;

import com.mapbox.mapboxsdk.geometry.LatLng;
import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interactors.JsonFormInteractor;
import com.vijay.jsonwizard.presenters.JsonFormFragmentPresenter;
import com.vijay.jsonwizard.utils.ValidationStatus;

import org.smartregister.reveal.BuildConfig;
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

    public RevealJsonFormFragmentPresenter(JsonFormFragment formFragment, JsonFormInteractor jsonFormInteractor) {
        super(formFragment, jsonFormInteractor);
        this.formFragment = formFragment;
        passwordDialog = PasswordDialogUtils.initPasswordDialog(formFragment.getActivity(), this);
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
                            validateUserLocation(formFragment.getActivity(), mapView);
                        } else {
                            onValidateUserLocation(true);
                        }
                    }
                }
            }

        }
        return validationStatus;
    }


    private void validateUserLocation(Activity activity, RevealMapView mapView) {
        if (ActivityCompat.checkSelfPermission(activity,
                Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            mapView.getFusedLocationClient().getLastLocation()
                    .addOnSuccessListener(location -> {
                        if (location != null) {
                            LatLng mapPosition = mapView.getMapboxMap().getCameraPosition().target;
                            double offset = mapPosition.distanceTo(new LatLng(location.getLatitude(), location.getLongitude()));
                            if (offset > BuildConfig.MY_LOCATION_BUFFER) {
                                onValidateUserLocation(false);
                            } else {
                                onValidateUserLocation(true);
                            }
                        } else {
                            onValidateUserLocation(false);
                        }
                    })
                    .addOnFailureListener(e -> {
                        onValidateUserLocation(false);
                    })
                    .addOnCanceledListener(() -> {
                        onValidateUserLocation(false);
                    });
        } else {
            onValidateUserLocation(false);
        }
    }

    private void onValidateUserLocation(boolean isValid) {
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

    @Override
    public void onPasswordVerified() {
        onValidateUserLocation(true);
    }
}
