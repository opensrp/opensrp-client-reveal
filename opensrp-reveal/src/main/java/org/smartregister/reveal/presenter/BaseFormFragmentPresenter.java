package org.smartregister.reveal.presenter;

import android.content.Context;
import android.content.Intent;
import android.support.v7.app.AlertDialog;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.mapbox.mapboxsdk.geometry.LatLng;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.json.JSONObject;
import org.smartregister.domain.Location;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.BaseFormFragmentContract;
import org.smartregister.reveal.model.BaseTaskDetails;
import org.smartregister.reveal.repository.RevealMappingHelper;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.Properties;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.PasswordDialogUtils;
import org.smartregister.reveal.view.FamilyRegisterActivity;
import org.smartregister.util.DateTimeTypeConverter;

import java.lang.ref.WeakReference;

import io.ona.kujaku.listeners.BaseLocationListener;

import static org.smartregister.reveal.util.Constants.DateFormat.EVENT_DATE_FORMAT_Z;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.Intervention.REGISTER_FAMILY;
import static org.smartregister.reveal.util.FamilyConstants.Intent.START_REGISTRATION;

/**
 * Created by samuelgithengi on 4/18/19.
 */
public class BaseFormFragmentPresenter extends BaseLocationListener implements BaseFormFragmentContract.Presenter {

    private final WeakReference<BaseFormFragmentContract.View> view;
    private AlertDialog passwordDialog;

    private ValidateUserLocationPresenter locationPresenter;

    protected RevealMappingHelper mappingHelper;

    private Location structure;

    private BaseTaskDetails taskDetails;

    private Context context;

    protected Gson gson = new GsonBuilder().setDateFormat(EVENT_DATE_FORMAT_Z)
            .registerTypeAdapter(DateTime.class, new DateTimeTypeConverter()).create();

    protected BaseFormFragmentPresenter(BaseFormFragmentContract.View view, Context context) {
        this.context = context;
        this.view = new WeakReference<>(view);
        passwordDialog = PasswordDialogUtils.initPasswordDialog(context, this);
        locationPresenter = new ValidateUserLocationPresenter(view, this);
        mappingHelper = new RevealMappingHelper();
    }

    protected boolean validateFarStructures() {
        return BuildConfig.VALIDATE_FAR_STRUCTURES;
    }

    private void validateUserLocation() {
        android.location.Location location = getView().getUserCurrentLocation();
        if (location == null) {
            locationPresenter.requestUserLocation();
        } else {
            locationPresenter.onGetUserLocation(location);
        }
    }

    @Override
    public void onPasswordVerified() {
        onLocationValidated();
    }

    @Override
    public void onLocationValidated() {

        String formName = getView().getJsonFormUtils().getFormName(null, taskDetails.getTaskCode());
        if (StringUtils.isBlank(formName)) {
            getView().displayError(R.string.opening_form_title, R.string.form_not_found);
        } else {
            JSONObject formJSON = getView().getJsonFormUtils().getFormJSON(context, formName, taskDetails, structure);
            getView().startForm(formJSON);
        }
        getView().hideProgressDialog();

    }

    @Override
    public LatLng getTargetCoordinates() {
        android.location.Location center = mappingHelper.getCenter(gson.toJson(structure.getGeometry()));
        return new LatLng(center.getLatitude(), center.getLongitude());
    }

    @Override
    public void requestUserPassword() {
        if (passwordDialog != null) {
            passwordDialog.show();
        }
    }

    @Override
    public ValidateUserLocationPresenter getLocationPresenter() {
        return locationPresenter;
    }

    protected BaseFormFragmentContract.View getView() {
        return view.get();
    }

    @Override
    public void onStructureFound(Location structure, BaseTaskDetails details) {
        this.structure = structure;
        this.taskDetails = details;
        if (IRS.equals(details.getTaskCode()) || MOSQUITO_COLLECTION.equals(details.getTaskCode()) ||
                LARVAL_DIPPING.equals(details.getTaskCode()) || REGISTER_FAMILY.equals(details.getTaskCode()) ||
                BEDNET_DISTRIBUTION.equals(details.getTaskCode()) || CASE_CONFIRMATION.equals(details.getTaskCode()) ||
                BLOOD_SCREENING.equals(details.getTaskCode())) {
            if (validateFarStructures()) {
                validateUserLocation();
            } else {
                onLocationValidated();
            }
        } else {
            onLocationValidated();
        }
    }

    public BaseTaskDetails getTaskDetails() {
        return taskDetails;
    }
}
