package org.smartregister.reveal.presenter;

import android.Manifest;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.location.Location;
import android.support.v4.app.ActivityCompat;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.Toast;

import com.google.android.gms.tasks.OnSuccessListener;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interactors.JsonFormInteractor;
import com.vijay.jsonwizard.presenters.JsonFormFragmentPresenter;
import com.vijay.jsonwizard.utils.ValidationStatus;

import org.smartregister.reveal.view.RevealMapView;
import org.smartregister.reveal.widget.GeoWidgetFactory;

import static org.smartregister.reveal.util.Constants.MY_LOCATION_BUFFER;

/**
 * Created by samuelgithengi on 1/30/19.
 */
public class RevealJsonFormFragmentPresenter extends JsonFormFragmentPresenter {

    private JsonFormFragment formFragment;

    public RevealJsonFormFragmentPresenter(JsonFormFragment formFragment, JsonFormInteractor jsonFormInteractor) {
        super(formFragment, jsonFormInteractor);
        this.formFragment = formFragment;
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
                        validateUserLocation(formFragment.getActivity(), mapView);
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
                    .addOnSuccessListener(activity, new OnSuccessListener<Location>() {
                        @Override
                        public void onSuccess(Location location) {
                            if (location != null) {
                                LatLng mapPosition = mapView.getMapboxMap().getCameraPosition().target;
                                double offset = mapPosition.distanceTo(new LatLng(location.getLatitude(), location.getLongitude()));
                                if (offset > MY_LOCATION_BUFFER) {
                                    onValidateUserLocation(false);
                                } else {
                                    onValidateUserLocation(true);
                                }
                            }
                            onValidateUserLocation(false);
                        }
                    });
        }
    }

    private void onValidateUserLocation(boolean isValid) {
        if (isValid) {
            Intent returnIntent = new Intent();
            getView().onFormFinish();
            returnIntent.putExtra("json", getView().getCurrentJsonState());
            returnIntent.putExtra(JsonFormConstants.SKIP_VALIDATION, Boolean.valueOf(formFragment.getMainView().getTag(com.vijay.jsonwizard.R.id.skip_validation).toString()));
            getView().finishWithResult(returnIntent);

        } else

        {
            requestUserPassword();
        }

    }

    @Override
    public void onSaveClick(LinearLayout mainView) {
        writeValuesAndValidate(mainView);

    }

    private void requestUserPassword() {
        Toast.makeText(formFragment.getContext(), "Requesting password for Add while far", Toast.LENGTH_LONG).show();
    }


}
