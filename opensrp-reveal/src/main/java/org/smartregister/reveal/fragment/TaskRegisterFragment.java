package org.smartregister.reveal.fragment;

import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.location.Location;
import android.os.Bundle;
import android.support.annotation.StringRes;
import android.support.v4.content.LocalBroadcastManager;
import android.view.View;
import android.widget.TextView;
import android.widget.Toast;

import org.json.JSONObject;
import org.smartregister.family.fragment.NoMatchDialogFragment;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.TaskRegisterAdapter;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.TaskRegisterFragmentContract;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.presenter.TaskRegisterFragmentPresenter;
import org.smartregister.reveal.util.AlertDialogUtils;
import org.smartregister.reveal.util.Constants.TaskRegister;
import org.smartregister.reveal.util.LocationUtils;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.util.Utils;
import org.smartregister.reveal.view.DrawerMenuView;
import org.smartregister.reveal.view.ListTasksActivity;
import org.smartregister.reveal.view.TaskRegisterActivity;
import org.smartregister.view.activity.BaseRegisterActivity;
import org.smartregister.view.fragment.BaseRegisterFragment;

import java.util.HashMap;
import java.util.List;
import java.util.Set;

import io.ona.kujaku.utils.Constants;

import static android.app.Activity.RESULT_CANCELED;
import static android.app.Activity.RESULT_OK;
import static org.smartregister.reveal.util.Constants.Action;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterFragment extends BaseRegisterFragment implements TaskRegisterFragmentContract.View, BaseDrawerContract.DrawerActivity {

    private TaskRegisterAdapter taskAdapter;

    private BaseDrawerContract.View drawerView;

    private RevealJsonFormUtils jsonFormUtils;
    private ProgressDialog progressDialog;
    private TextView interventionTypeTv;

    private LocationUtils locationUtils;

    private boolean hasRequestedLocation;

    private RefreshRegisterReceiver refreshRegisterReceiver = new RefreshRegisterReceiver();

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        drawerView = new DrawerMenuView(this);
        drawerView.initializeDrawerLayout();
        progressDialog = new ProgressDialog(getContext());
        progressDialog.setCancelable(false);
    }

    @Override
    protected int getLayout() {
        return R.layout.fragment_task_register;
    }

    public void initializeAdapter(Set<org.smartregister.configurableviews.model.View> visibleColumns) {
        taskAdapter = new TaskRegisterAdapter(getActivity(), registerActionHandler);
        clientsView.setAdapter(taskAdapter);
    }

    @Override
    public void setupViews(View view) {
        super.setupViews(view);
        interventionTypeTv = view.findViewById(R.id.intervention_type);
        if (getActivity() != null) {
            interventionTypeTv.setText(
                    getActivity().getIntent().getStringExtra(TaskRegister.INTERVENTION_TYPE));
        }
        view.findViewById(R.id.txt_map_label).setOnClickListener(v -> startMapActivity());
        view.findViewById(R.id.drawerMenu).setOnClickListener(v -> drawerView.openDrawerLayout());
        drawerView.onResume();
    }

    private void startMapActivity() {
        Intent intent = new Intent(getContext(), ListTasksActivity.class);
        startActivity(intent);
    }

    @Override
    public Location getLastLocation() {
        if (getActivity() != null && getActivity().getIntent().getExtras() != null) {
            return getActivity().getIntent().getExtras().getParcelable(TaskRegister.LAST_USER_LOCATION);
        } else {
            return null;
        }
    }

    @Override
    protected void initializePresenter() {
        presenter = new TaskRegisterFragmentPresenter(this, TaskRegister.VIEW_IDENTIFIER);
        locationUtils = new LocationUtils(getContext());
        locationUtils.requestLocationUpdates(getPresenter());
    }

    @Override
    public void setUniqueID(String qrCode) {
        if (this.getSearchView() != null) {
            this.getSearchView().setText(qrCode);
        }
    }

    @Override
    public void setAdvancedSearchFormData(HashMap<String, String> hashMap) {//not used
    }

    @Override
    protected String getMainCondition() {
        return null;//not used
    }

    @Override
    protected String getDefaultSortQuery() {
        return null;//not used
    }

    @Override
    protected void startRegistration() {////not used on reveal/ adding points done on map
    }

    @Override
    protected void onViewClicked(View view) {

        TaskDetails details = (TaskDetails) view.getTag(R.id.task_details);
        getPresenter().onTaskSelected(details);
    }

    @Override
    public void showNotFoundPopup(String opensrpId) {
        if (this.getActivity() != null) {
            NoMatchDialogFragment.launchDialog((BaseRegisterActivity) this.getActivity(), "dialog", opensrpId);
        }
    }

    private TaskRegisterFragmentPresenter getPresenter() {
        return (TaskRegisterFragmentPresenter) presenter;
    }

    @Override
    public void setTotalPatients() {
        //do nothing using @link setTotalTasks(structuresWithinBuffer)
    }


    @Override
    public void setTotalTasks(int structuresWithinBuffer) {
        if (headerTextDisplay != null) {
            headerTextDisplay.setText(getResources().getQuantityString(R.plurals.structures,
                    taskAdapter.getItemCount(), structuresWithinBuffer, Utils.getLocationBuffer(), taskAdapter.getItemCount()));

            filterRelativeLayout.setVisibility(View.GONE);
        }
    }

    public void setTaskDetails(List<TaskDetails> tasks) {
        taskAdapter.setTaskDetails(tasks);
    }

    @Override
    public void displayNotification(int title, int message, Object... formatArgs) {
        setRefreshList(false);
        AlertDialogUtils.displayNotification(getContext(), title, message, formatArgs);
    }

    @Override
    public void startForm(JSONObject formName) {
        ((TaskRegisterActivity) getActivity()).startFormActivity(formName);
    }

    @Override
    public void onDestroy() {
        getPresenter().onDestroy();
        super.onDestroy();
    }

    @Override
    public void onDrawerClosed() {
        getPresenter().onDrawerClosed();

    }

    @Override
    public RevealJsonFormUtils getJsonFormUtils() {
        return jsonFormUtils;
    }

    @Override
    public Location getUserCurrentLocation() {
        return locationUtils.getLastLocation();
    }

    @Override
    public void requestUserLocation() {
        hasRequestedLocation = true;
        locationUtils.checkLocationSettingsAndStartLocationServices(getActivity(), getPresenter());
    }

    @Override
    public void displayToast(String message) {
        Toast.makeText(getContext(), message, Toast.LENGTH_LONG).show();
    }

    @Override
    public LocationUtils getLocationUtils() {
        return locationUtils;
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
    public void setInventionType(int interventionLabel) {
        interventionTypeTv.setText(getString(interventionLabel));
    }

    public void setJsonFormUtils(RevealJsonFormUtils jsonFormUtils) {
        this.jsonFormUtils = jsonFormUtils;
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == Constants.RequestCode.LOCATION_SETTINGS && hasRequestedLocation) {
            if (resultCode == RESULT_OK) {
                locationUtils.requestLocationUpdates(getPresenter());
                getPresenter().getLocationPresenter().waitForUserLocation();
            } else if (resultCode == RESULT_CANCELED) {
                getPresenter().getLocationPresenter().onGetUserLocationFailed();
            }
            hasRequestedLocation = false;
        }
    }


    @Override
    public void onPause() {
        if (getContext() != null)
            LocalBroadcastManager.getInstance(getContext()).unregisterReceiver(refreshRegisterReceiver);
        super.onPause();
    }

    @Override
    public void onResume() {
        super.onResume();
        if (getContext() != null) {
            IntentFilter filter = new IntentFilter(Action.STRUCTURE_TASK_SYNCED);
            LocalBroadcastManager.getInstance(getContext()).registerReceiver(refreshRegisterReceiver, filter);
        }
    }

    private class RefreshRegisterReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            getPresenter().initializeQueries(getMainCondition());
        }
    }
}
