package org.smartregister.reveal.presenter;

import android.support.annotation.NonNull;
import android.support.v7.app.AlertDialog;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.mapbox.geojson.Feature;
import com.mapbox.mapboxsdk.geometry.LatLng;

import org.joda.time.DateTime;
import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.PasswordRequestCallback;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.contract.UserLocationContract;
import org.smartregister.reveal.interactor.StructureTasksInteractor;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.repository.RevealMappingHelper;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.PasswordDialogUtils;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.util.DateTimeTypeConverter;

import java.lang.ref.WeakReference;
import java.util.List;

import io.ona.kujaku.listeners.BaseLocationListener;

import static org.smartregister.reveal.contract.StructureTasksContract.Interactor;
import static org.smartregister.reveal.contract.StructureTasksContract.Presenter;
import static org.smartregister.reveal.util.Constants.DateFormat.EVENT_DATE_FORMAT_Z;
import static org.smartregister.reveal.util.Constants.Intervention.BCC;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.Intervention.REGISTER_FAMILY;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class StructureTasksPresenter implements Presenter, PasswordRequestCallback,
        UserLocationContract.UserLocationCallback {

    private final RevealMappingHelper mappingHelper;
    private WeakReference<StructureTasksContract.View> view;

    private Interactor interactor;

    private PreferencesUtil prefsUtil;
    private Location structure;

    private ValidateUserLocationPresenter locationPresenter;
    private StructureTaskDetails details;

    private Gson gson = new GsonBuilder().setDateFormat(EVENT_DATE_FORMAT_Z)
            .registerTypeAdapter(DateTime.class, new DateTimeTypeConverter()).create();
    private AlertDialog passwordDialog;
    private android.location.Location lastLocation;


    public StructureTasksPresenter(StructureTasksContract.View view) {
        this.view = new WeakReference<>(view);
        interactor = new StructureTasksInteractor(this);
        prefsUtil = PreferencesUtil.getInstance();
        locationPresenter = new ValidateUserLocationPresenter(view, this);
        passwordDialog = PasswordDialogUtils.initPasswordDialog(view.getContext(), this);
        mappingHelper = new RevealMappingHelper();
    }

    @Override
    public void findTasks(String structureId) {
        interactor.findTasks(structureId, prefsUtil.getCurrentCampaignId());
    }

    @Override
    public void onTasksFound(List<StructureTaskDetails> taskDetailsList) {
        getView().getAdapter().setTaskDetailsList(taskDetailsList);
    }

    @Override
    public void onTaskSelected(StructureTaskDetails details) {
        if (details != null) {
            //TODO remove this condition once BCC
            if (BCC.equals(details.getTaskCode())) {
                getView().displayToast(String.format("To open %s form for %s",
                        details.getTaskCode(), details.getTaskId()));

            } else if (Task.TaskStatus.COMPLETED.name().equals(details.getTaskStatus())) {
                getView().displayToast("Task Completed");
            } else {
                getView().showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
                interactor.getStructure(details);
            }
        }
    }

    @Override
    public void onStructureFound(Location structure, StructureTaskDetails details) {
        this.structure = structure;
        this.details = details;
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

    private boolean validateFarStructures() {
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

    public StructureTasksContract.View getView() {
        return view.get();
    }

    @Override
    public void onPasswordVerified() {
        onLocationValidated();
    }

    @Override
    public void onLocationValidated() {
        String formName = RevealJsonFormUtils.getFormName(null, details.getTaskCode());
        JSONObject formJSON = getView().getJsonFormUtils().getFormJSON(getView().getContext(), formName, details, structure);
        getView().startForm(formJSON);
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

    @Override
    public void saveJsonForm(String json) {
        getView().showProgressDialog(R.string.saving_title, R.string.saving_message);
        interactor.saveJsonForm(json);
    }

    @Override
    public void onFormSaved(@NonNull String structureId, @NonNull Task.TaskStatus taskStatus, @NonNull String businessStatus, String interventionType) {
        getView().hideProgressDialog();
    }

    @Override
    public void onStructureAdded(Feature feature, JSONArray featureCoordinates) {//not used
    }

    @Override
    public void onFormSaveFailure(String eventType) {
        getView().hideProgressDialog();//register will refresh on resume
    }
}
