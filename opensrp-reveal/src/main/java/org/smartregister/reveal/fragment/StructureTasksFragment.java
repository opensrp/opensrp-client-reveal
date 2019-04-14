package org.smartregister.reveal.fragment;

import android.app.Activity;
import android.app.ProgressDialog;
import android.content.IntentSender;
import android.location.Location;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.annotation.StringRes;
import android.support.v4.app.Fragment;
import android.support.v7.widget.RecyclerView;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.Toast;

import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.location.LocationSettingsResult;
import com.google.android.gms.location.LocationSettingsStatusCodes;

import org.json.JSONObject;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.StructureTaskAdapter;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.presenter.StructureTasksPresenter;
import org.smartregister.reveal.util.LocationUtils;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.util.Utils;

import io.ona.kujaku.listeners.BaseLocationListener;
import io.ona.kujaku.utils.Constants;
import io.ona.kujaku.utils.LocationSettingsHelper;
import io.ona.kujaku.utils.LogUtil;

/**
 * Created by samuelgithengi on 4/8/19.
 */
public class StructureTasksFragment extends Fragment implements StructureTasksContract.View {

    private static final String TAG = StructureTasksFragment.class.getName();
    private RecyclerView taskRecyclerView;
    private StructureTaskAdapter adapter;

    private StructureTasksContract.Presenter presenter;
    private ProgressDialog progressDialog;

    private RevealJsonFormUtils jsonFormUtils;

    private LocationUtils locationUtils;
    private boolean hasRequestedLocation;

    public static StructureTasksFragment newInstance(Bundle bundle) {
        StructureTasksFragment fragment = new StructureTasksFragment();
        fragment.setPresenter(new StructureTasksPresenter(fragment));
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        presenter = new StructureTasksPresenter(this);
        locationUtils = new LocationUtils(getActivity());
        locationUtils.requestLocationUpdates(new BaseLocationListener());
        jsonFormUtils = new RevealJsonFormUtils();
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_structure_task, container, false);
        setUpViews(view);
        initializeAdapter();
        return view;
    }

    private void initializeAdapter() {
        adapter = new StructureTaskAdapter(onClickListener);
        taskRecyclerView.setAdapter(adapter);
    }

    private void setUpViews(View view) {
        TextView interventionType = view.findViewById(R.id.intervention_type);
        interventionType.setText(getString(Utils.getInterventionLabel()));
        taskRecyclerView = view.findViewById(R.id.task_recyclerView);

        progressDialog = new ProgressDialog(getContext());
        progressDialog.setCancelable(false);
    }

    private View.OnClickListener onClickListener = new View.OnClickListener() {
        @Override
        public void onClick(View view) {
            StructureTaskDetails details = (StructureTaskDetails) view.getTag(R.id.task_details);
            presenter.onTaskSelected(details);
        }
    };

    @Override
    public StructureTaskAdapter getAdapter() {
        return adapter;
    }

    @Override
    public void setStructure(String structureId) {
        presenter.findTasks(structureId);
    }

    @Override
    public void displayToast(String message) {
        Toast.makeText(getContext(), message, Toast.LENGTH_LONG).show();
    }

    public void setPresenter(StructureTasksContract.Presenter presenter) {
        this.presenter = presenter;
    }

    @Override
    public void showProgressDialog(@StringRes int title, @StringRes int message) {
        if (progressDialog != null) {
            progressDialog.setTitle(title);
            progressDialog.setMessage(getString(message));
            progressDialog.show();
        }
    }

    @Override
    public void hideProgressDialog() {
        if (progressDialog != null) {
            progressDialog.dismiss();
        }
    }

    @Override
    public void requestUserLocation() {
        hasRequestedLocation = true;
        checkLocationSettingsAndStartLocationServices();
    }

    private void checkLocationSettingsAndStartLocationServices() {
        if (getContext() instanceof Activity) {
            Activity activity = (Activity) getContext();

            LocationSettingsHelper.checkLocationEnabled(activity, new ResultCallback<LocationSettingsResult>() {
                @Override
                public void onResult(LocationSettingsResult result) {
                    final Status status = result.getStatus();

                    switch (status.getStatusCode()) {
                        case LocationSettingsStatusCodes.SUCCESS:
                            Log.i(TAG, "All location settings are satisfied.");
                            locationUtils.requestLocationUpdates(new BaseLocationListener());
                            break;
                        case LocationSettingsStatusCodes.RESOLUTION_REQUIRED:
                            Log.i(TAG, "Location settings are not satisfied. Show the user a dialog to upgrade location settings");

                            try {
                                status.startResolutionForResult(activity, Constants.RequestCode.LOCATION_SETTINGS);
                            } catch (IntentSender.SendIntentException e) {
                                Log.i(TAG, "PendingIntent unable to execute request.");
                            }
                            break;
                        case LocationSettingsStatusCodes.SETTINGS_CHANGE_UNAVAILABLE:
                            Log.e(TAG, "Location settings are inadequate, and cannot be fixed here. Dialog cannot be created.");
                            break;

                        default:
                            Log.e(TAG, "Unknown status code returned after checking location settings");
                            break;
                    }
                }
            });
        } else {
            LogUtil.e(TAG, "KujakuMapView is not started in an Activity and can therefore not start location services");
        }
    }

    @Override
    public Location getUserCurrentLocation() {
        return locationUtils.getLastLocation();
    }

    @Override
    public RevealJsonFormUtils getJsonFormUtils() {
        return jsonFormUtils;
    }

    @Override
    public void startForm(JSONObject formJSON) {
        jsonFormUtils.startJsonForm(formJSON, getActivity());
    }

}
