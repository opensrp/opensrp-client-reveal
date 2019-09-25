package org.smartregister.reveal.fragment;

import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.location.Location;
import android.os.Bundle;
import android.support.annotation.StringRes;
import android.support.v4.content.LocalBroadcastManager;
import android.support.v7.widget.CardView;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.fragment.NoMatchDialogFragment;
import org.smartregister.family.util.DBConstants;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.TaskRegisterAdapter;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.TaskRegisterFragmentContract;
import org.smartregister.reveal.model.BaseTaskDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.presenter.TaskRegisterFragmentPresenter;
import org.smartregister.reveal.task.IndicatorsCalculatorTask;
import org.smartregister.reveal.util.AlertDialogUtils;
import org.smartregister.reveal.util.Constants.Properties;
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

    private CardView indicatorsCardView;

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

        initializeProgressIndicatorViews(view);
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
        getPresenter().onTaskSelected(details, view.getId() == R.id.task_action);
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
        new IndicatorsCalculatorTask(getActivity(), tasks).execute();
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
    public void displayError(int title, int message) {
        new AlertDialog.Builder(getActivity()).setTitle(title).setMessage(message).create().show();
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

    @Override
    public void registerFamily(BaseTaskDetails taskDetails) {
        ((TaskRegisterActivity) getActivity()).startFamilyRegistration(taskDetails);
    }

    @Override
    public void openFamilyProfile(CommonPersonObjectClient family, BaseTaskDetails taskDetails) {
        Intent intent = new Intent(getContext(), org.smartregister.family.util.Utils.metadata().profileActivity);
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.FAMILY_BASE_ENTITY_ID, family.getCaseId());
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.FAMILY_HEAD, org.smartregister.family.util.Utils.getValue(family.getColumnmaps(), DBConstants.KEY.FAMILY_HEAD, false));
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.PRIMARY_CAREGIVER, org.smartregister.family.util.Utils.getValue(family.getColumnmaps(), DBConstants.KEY.PRIMARY_CAREGIVER, false));
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.FAMILY_NAME, org.smartregister.family.util.Utils.getValue(family.getColumnmaps(), DBConstants.KEY.FIRST_NAME, false));
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.GO_TO_DUE_PAGE, false);


        intent.putExtra(Properties.LOCATION_UUID, taskDetails.getStructureId());
        intent.putExtra(Properties.TASK_IDENTIFIER, taskDetails.getTaskId());
        intent.putExtra(Properties.TASK_BUSINESS_STATUS, taskDetails.getBusinessStatus());
        intent.putExtra(Properties.TASK_STATUS, taskDetails.getTaskStatus());

        startActivity(intent);

    }

    @Override
    public void displayIndexCaseDetails(JSONObject indexCase) {
        ((TaskRegisterActivity) getActivity()).displayIndexCaseFragment(indexCase);
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
        setViewVisibility(indicatorsCardView, false);
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

    private void initializeProgressIndicatorViews(View view) {

        indicatorsCardView = view.findViewById(R.id.indicators_card_view);
        indicatorsCardView.findViewById(R.id.btn_collapse_indicators_card_view).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                setViewVisibility(indicatorsCardView, false);
            }
        });

        LinearLayout progressIndicatorsGroupView = view.findViewById(R.id.progressIndicatorsGroupView);
        progressIndicatorsGroupView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                openIndicatorsCardView();
            }
        });
    }

    private void openIndicatorsCardView() {

        setViewVisibility(indicatorsCardView, true);
    }


    private void setViewVisibility(View view, boolean isVisible) {
        view.setVisibility(isVisible ? View.VISIBLE : View.GONE);
    }

}
