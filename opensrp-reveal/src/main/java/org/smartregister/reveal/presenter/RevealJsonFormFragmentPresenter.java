package org.smartregister.reveal.presenter;

import android.content.Intent;
import android.location.Location;
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
import org.smartregister.reveal.activity.RevealJsonFormActivity;
import org.smartregister.reveal.contract.PasswordRequestCallback;
import org.smartregister.reveal.contract.UserLocationContract.UserLocationCallback;
import org.smartregister.reveal.util.PasswordDialogUtils;
import org.smartregister.reveal.view.RevealMapView;
import org.smartregister.reveal.widget.GeoWidgetFactory;

/**
 * Created by samuelgithengi on 1/30/19.
 */
public class RevealJsonFormFragmentPresenter extends JsonFormFragmentPresenter implements PasswordRequestCallback, UserLocationCallback {

    private JsonFormFragment formFragment;

    private AlertDialog passwordDialog;

    private RevealJsonFormActivity jsonFormView;

    private RevealMapView mapView;

    private ValidateUserLocationPresenter locationPresenter;


    public RevealJsonFormFragmentPresenter(JsonFormFragment formFragment, JsonFormInteractor jsonFormInteractor) {
        super(formFragment, jsonFormInteractor);
        this.formFragment = formFragment;
        passwordDialog = PasswordDialogUtils.initPasswordDialog(formFragment.getActivity(), this);
        jsonFormView = (RevealJsonFormActivity) formFragment.getActivity();
        locationPresenter = new ValidateUserLocationPresenter(jsonFormView, this);

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
                            onLocationValidated();
                        }
                    }
                }
            }
        }
        return validationStatus;
    }

    private void validateUserLocation(RevealMapView mapView) {
        this.mapView = mapView;
        Location location = jsonFormView.getUserCurrentLocation();
        if (location != null) {
            locationPresenter.onGetUserLocation(location);
        } else {
            locationPresenter.requestUserLocation();
        }
    }

    @Override
    public void onSaveClick(LinearLayout mainView) {
        writeValuesAndValidate(mainView);
    }

    @Override
    public void onLocationValidated() {
        jsonFormView.hideProgressDialog();
        Intent returnIntent = new Intent();
        getView().onFormFinish();
        returnIntent.putExtra("json", getView().getCurrentJsonState());
        returnIntent.putExtra(JsonFormConstants.SKIP_VALIDATION, Boolean.valueOf(formFragment.getMainView().getTag(com.vijay.jsonwizard.R.id.skip_validation).toString()));
        getView().finishWithResult(returnIntent);

    }

    @Override
    public LatLng getTargetCoordinates() {
        return mapView.getMapboxMap().getCameraPosition().target;
    }

    @Override
    public void requestUserPassword() {
        if (passwordDialog != null) {
            passwordDialog.show();
        }
    }

    @Override
    public void onPasswordVerified() {
        onLocationValidated();
    }


    public RevealMapView getMapView() {
        return mapView;
    }

    @Override
    public ValidateUserLocationPresenter getLocationPresenter() {
        return locationPresenter;
    }
}
