package org.smartregister.reveal.fragment;

import android.content.Intent;
import android.location.Location;
import android.os.Bundle;
import android.view.View;
import android.widget.TextView;

import org.smartregister.family.fragment.NoMatchDialogFragment;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.TaskRegisterAdapter;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.TaskRegisterFragmentContract;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.presenter.TaskRegisterFragmentPresenter;
import org.smartregister.reveal.util.AlertDialogUtils;
import org.smartregister.reveal.util.Constants.TaskRegister;
import org.smartregister.reveal.util.Utils;
import org.smartregister.reveal.view.DrawerMenuView;
import org.smartregister.reveal.view.ListTasksActivity;
import org.smartregister.view.activity.BaseRegisterActivity;
import org.smartregister.view.fragment.BaseRegisterFragment;

import java.util.HashMap;
import java.util.List;
import java.util.Set;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterFragment extends BaseRegisterFragment implements TaskRegisterFragmentContract.View, BaseDrawerContract.DrawerActivity {

    private TaskRegisterAdapter taskAdapter;


    private BaseDrawerContract.View drawerView;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        drawerView = new DrawerMenuView(this);
        drawerView.initializeDrawerLayout();
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
        if (getActivity() != null) {
            ((TextView) view.findViewById(R.id.intervention_type)).setText(
                    getActivity().getIntent().getStringExtra(TaskRegister.INTERVENTION_TYPE));
        }
        view.findViewById(R.id.txt_map_label).setOnClickListener(v -> startMapActivity());
        view.findViewById(R.id.drawerMenu).setOnClickListener(v -> drawerView.openDrawerLayout());
        drawerView.onInitializeDrawerLayout();
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
    public void onDestroy() {
        getPresenter().onDestroy();
        super.onDestroy();
    }

    @Override
    public void onDrawerClosed() {
        getPresenter().onDrawerClosed();
    }
}
