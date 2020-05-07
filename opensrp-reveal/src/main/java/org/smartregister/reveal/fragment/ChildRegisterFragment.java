package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.Toast;

import com.vijay.jsonwizard.domain.Form;

import org.json.JSONObject;
import org.smartregister.domain.FetchStatus;
import org.smartregister.domain.ResponseErrorStatus;
import org.smartregister.receiver.SyncStatusBroadcastReceiver;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.ChildRegisterAdapter;
import org.smartregister.reveal.adapter.GroupedListableAdapter;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.ChildRegisterFragmentContract;
import org.smartregister.reveal.contract.FormProcessor;
import org.smartregister.reveal.model.Child;
import org.smartregister.reveal.model.ChildModel;
import org.smartregister.reveal.presenter.ChildRegisterFragmentPresenter;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.view.ChildProfileActivity;
import org.smartregister.reveal.view.ChildRegisterActivity;
import org.smartregister.reveal.view.DrawerMenuView;
import org.smartregister.reveal.viewholder.GroupedListableViewHolder;
import org.smartregister.util.Utils;
import org.smartregister.view.fragment.BaseListFragment;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.Callable;

import timber.log.Timber;

public class ChildRegisterFragment extends BaseListFragment<Child> implements ChildRegisterFragmentContract.View, BaseDrawerContract.DrawerActivity, SyncStatusBroadcastReceiver.SyncStatusListener,
        FormProcessor.Requester {
    public static final String TAG = "ChildRegisterFragment";

    private BaseDrawerContract.View drawerView;
    private TextView tvTitle;
    @Nullable
    private HashMap<String, List<String>> filterAndSearch;
    private String locationName = "";
    private TextView searchTextView;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        Bundle bundle = getArguments();
        if (bundle != null) {
            Serializable serializable = bundle.getSerializable(Constants.ChildFilter.FILTER_PAYLOAD);
            if (serializable != null)
                filterAndSearch = (HashMap<String, List<String>>) serializable;
        }
        return super.onCreateView(inflater, container, savedInstanceState);
    }

    @Override
    public void bindLayout() {
        super.bindLayout();
        drawerView = new DrawerMenuView(this);

        drawerView.initializeDrawerLayout();
        view.findViewById(R.id.drawerMenu).setOnClickListener(v -> drawerView.openDrawerLayout());

        drawerView.onResume();

        tvTitle = view.findViewById(R.id.intervention_type);
        tvTitle.setText(drawerView.getOperationalArea());

        TextView mapText = view.findViewById(R.id.txt_map_label);
        mapText.setText(R.string.label_add);
        mapText.setOnClickListener(v -> startChildRegistrationForm());

        searchTextView = view.findViewById(R.id.edt_search);
        searchTextView.setHint(R.string.search_students);
        searchTextView.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                searchPresenter(s.toString());
            }

            @Override
            public void afterTextChanged(Editable s) {

            }
        });

        view.findViewById(R.id.filter_text_view).setOnClickListener(v -> openFilterFragment());
    }

    private void searchPresenter(String searchText) {
        loadPresenter().search(locationName, filterAndSearch, searchText);
    }

    @NonNull
    @Override
    protected Callable<List<Child>> onStartCallable(@Nullable Bundle bundle) {
        ChildModel model = presenter.getModel();
        return () -> model.searchAndFilter(locationName, filterAndSearch, locationName);
    }

    @Override
    protected int getRootLayout() {
        return R.layout.fragment_child_register;
    }

    @Override
    protected int getRecyclerViewID() {
        return R.id.recycler_view;
    }

    @Override
    protected int getProgressBarID() {
        return R.id.client_list_progress;
    }

    @Override
    public void onListItemClicked(Child child, int layoutID) {
        if (layoutID == R.id.linearLayoutAction || layoutID == R.id.btnDue) {
            startRecordMDAForm(child.getBaseEntityID());
        } else {
            ChildProfileActivity.startMe(getActivity(), child.getBaseEntityID());
        }
    }

    @Override
    public void onFetchError(Exception e) {
        Toast.makeText(getContext(), "An error occurred, handle it bro", Toast.LENGTH_SHORT).show();
        Timber.e(e);
    }

    @NonNull
    @Override
    public GroupedListableAdapter<Child, GroupedListableViewHolder<Child>> adapter() {
        return new ChildRegisterAdapter(list, this);
    }

    @NonNull
    @Override
    public ChildRegisterFragmentPresenter loadPresenter() {
        if (presenter == null) {
            presenter = new ChildRegisterFragmentPresenter()
                    .with(this)
                    .withModel(new ChildModel());
        }
        return (ChildRegisterFragmentPresenter) presenter;
    }

    @Override
    public void onDrawerClosed() {
        // to do -> re render the details
        locationName = drawerView.getOperationalArea();
        tvTitle.setText(locationName);
        searchPresenter(locationName);
    }

    @Override
    public void openFilterFragment() {
        ChildRegisterActivity.startFragment(getActivity(), ChildFilterFragment.TAG, null, false);
    }

    @Override
    public void startChildRegistrationForm() {
        loadPresenter().startChildRegistrationForm(getContext());
    }

    @Override
    public void startRecordMDAForm(String baseEntityID) {
        loadPresenter().startMDAForm(getContext(), baseEntityID);
    }

    @Override
    public void startJsonForm(JSONObject jsonObject) {
        Form form = new Form();
        form.setName(getString(R.string.add_student));
        form.setActionBarBackground(org.smartregister.family.R.color.family_actionbar);
        form.setNavigationBackground(org.smartregister.family.R.color.family_navigation);
        form.setHomeAsUpIndicator(org.smartregister.family.R.mipmap.ic_cross_white);
        form.setPreviousLabel(getResources().getString(org.smartregister.family.R.string.back));
        form.setWizard(false);

        getHostFormProcessor().startForm(jsonObject, form, this);
    }


    @Override
    public void onResume() {
        if (!(getActivity() instanceof FormProcessor.Host))
            throw new IllegalStateException("Host activity must implement org.smartregister.reveal.contract.FormProcessor.Host");

        SyncStatusBroadcastReceiver.getInstance().addSyncStatusListener(this);
        super.onResume();
    }

    @Override
    public void onPause() {
        SyncStatusBroadcastReceiver.getInstance().removeSyncStatusListener(this);
        super.onPause();
    }

    @Override
    public void onSyncStart() {
        refreshSyncStatusViews(null);
    }

    @Override
    public void onSyncInProgress(FetchStatus fetchStatus) {
        refreshSyncStatusViews(fetchStatus);
    }

    @Override
    public void onSyncComplete(FetchStatus fetchStatus) {
        refreshSyncStatusViews(fetchStatus);
    }

    private void refreshSyncStatusViews(FetchStatus fetchStatus) {
        if (SyncStatusBroadcastReceiver.getInstance().isSyncing()) {
            Utils.showShortToast(getActivity(), getString(org.smartregister.R.string.syncing));
            Timber.i(getString(org.smartregister.R.string.syncing));
        } else {
            if (fetchStatus != null) {

                if (fetchStatus.equals(FetchStatus.fetchedFailed)) {
                    if (fetchStatus.displayValue().equals(ResponseErrorStatus.malformed_url.name())) {
                        Utils.showShortToast(getActivity(), getString(org.smartregister.R.string.sync_failed_malformed_url));
                        Timber.i(getString(org.smartregister.R.string.sync_failed_malformed_url));
                    } else if (fetchStatus.displayValue().equals(ResponseErrorStatus.timeout.name())) {
                        Utils.showShortToast(getActivity(), getString(org.smartregister.R.string.sync_failed_timeout_error));
                        Timber.i(getString(org.smartregister.R.string.sync_failed_timeout_error));
                    } else {
                        Utils.showShortToast(getActivity(), getString(org.smartregister.R.string.sync_failed));
                        Timber.i(getString(org.smartregister.R.string.sync_failed));
                    }

                } else if (fetchStatus.equals(FetchStatus.fetched)
                        || fetchStatus.equals(FetchStatus.nothingFetched)) {

                    searchPresenter(searchTextView.getText().toString());

                    Utils.showShortToast(getActivity(), getString(org.smartregister.R.string.sync_complete));
                    Timber.i(getString(org.smartregister.R.string.sync_complete));

                } else if (fetchStatus.equals(FetchStatus.noConnection)) {

                    Utils.showShortToast(getActivity(), getString(org.smartregister.R.string.sync_failed_no_internet));
                    Timber.i(getString(org.smartregister.R.string.sync_failed_no_internet));
                }
            } else {
                Timber.i("Fetch Status NULL");
            }

        }
    }

    @Override
    public void onFormProcessingResult(String jsonForm) {
        loadPresenter().saveChild(jsonForm);
    }

    @Override
    public FormProcessor.Host getHostFormProcessor() {
        return (FormProcessor.Host) getActivity();
    }
}
