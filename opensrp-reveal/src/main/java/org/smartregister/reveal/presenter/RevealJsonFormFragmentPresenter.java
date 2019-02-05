package org.smartregister.reveal.presenter;

import android.Manifest;
import android.app.Activity;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.location.Location;
import android.support.v4.app.ActivityCompat;
import android.support.v7.app.AlertDialog;
import android.text.method.HideReturnsTransformationMethod;
import android.text.method.PasswordTransformationMethod;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.Toast;

import com.google.android.gms.tasks.OnSuccessListener;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interactors.JsonFormInteractor;
import com.vijay.jsonwizard.presenters.JsonFormFragmentPresenter;
import com.vijay.jsonwizard.utils.ValidationStatus;

import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.view.RevealMapView;
import org.smartregister.reveal.widget.GeoWidgetFactory;

import static org.smartregister.reveal.util.Constants.MY_LOCATION_BUFFER;

/**
 * Created by samuelgithengi on 1/30/19.
 */
public class RevealJsonFormFragmentPresenter extends JsonFormFragmentPresenter {

    private JsonFormFragment formFragment;

    private AlertDialog passwordDialog;

    public RevealJsonFormFragmentPresenter(JsonFormFragment formFragment, JsonFormInteractor jsonFormInteractor) {
        super(formFragment, jsonFormInteractor);
        this.formFragment = formFragment;
        initPasswordDialog(formFragment.getActivity());
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
        } else {
            Log.w(RevealJsonFormFragmentPresenter.class.getName(), "password dialog is null");
        }

    }

    private void initPasswordDialog(Activity activity) {
        if (activity == null) {
            return;
        }
        LayoutInflater inflater = activity.getLayoutInflater();
        View dialogView = inflater.inflate(R.layout.dialog_request_password, null);

        passwordDialog = new AlertDialog.Builder(activity)
                .setTitle(R.string.request_password_title)
                .setView(dialogView)
                .setNegativeButton(R.string.cancel, null)
                .setPositiveButton(R.string.ok, null)
                .setCancelable(false)
                .create();

        final EditText adminPassEditText = dialogView.findViewById(R.id.admin_pass);
        ((CheckBox) dialogView.findViewById(R.id.show_password_checkbox)).setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                if (isChecked)
                    adminPassEditText.setTransformationMethod(HideReturnsTransformationMethod.getInstance());
                else
                    adminPassEditText.setTransformationMethod(PasswordTransformationMethod.getInstance());
            }
        });

        passwordDialog.setOnShowListener(new DialogInterface.OnShowListener() {
            @Override
            public void onShow(DialogInterface dialogInterface) {

                passwordDialog.getButton(AlertDialog.BUTTON_POSITIVE).setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View view) {
                        if (!adminPassEditText.getText().toString().equals(BuildConfig.ADMIN_PASSWORD_NOT_NEAR_STRUCTURES)) {
                            Toast.makeText(activity, R.string.wrong_admin_password, Toast.LENGTH_LONG).show();
                            adminPassEditText.setError(activity.getString(R.string.wrong_admin_password));
                        } else {
                            adminPassEditText.setError(null);
                            passwordDialog.dismiss();
                            onValidateUserLocation(true);
                        }
                    }
                });

                passwordDialog.getButton(AlertDialog.BUTTON_NEGATIVE).setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        passwordDialog.dismiss();
                        passwordDialog.dismiss();
                    }
                });
            }
        });
    }

}
