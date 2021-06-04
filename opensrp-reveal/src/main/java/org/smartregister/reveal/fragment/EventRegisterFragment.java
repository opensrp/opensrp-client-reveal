package org.smartregister.reveal.fragment;


import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.StringRes;

import org.json.JSONObject;
import org.smartregister.cursoradapter.RecyclerViewPaginatedAdapter;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.EventRegisterContract;
import org.smartregister.reveal.model.EventRegisterDetails;
import org.smartregister.reveal.model.FilterConfiguration;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.presenter.EventRegisterFragmentPresenter;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.EventType;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.view.DrawerMenuView;
import org.smartregister.reveal.view.EventRegisterActivity;
import org.smartregister.reveal.view.FilterTasksActivity;
import org.smartregister.reveal.view.ListTasksActivity;
import org.smartregister.reveal.viewholder.EventViewHolder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import static android.app.Activity.RESULT_OK;
import static org.smartregister.reveal.util.Constants.Filter.FILTER_CONFIGURATION;
import static org.smartregister.reveal.util.Constants.Filter.FILTER_SORT_PARAMS;
import static org.smartregister.reveal.util.Constants.RequestCode.REQUEST_CODE_FILTER_TASKS;

/**
 * Created by samuelgithengi on 7/30/20.
 */
public class EventRegisterFragment extends BaseDrawerRegisterFragment implements EventRegisterContract.View, BaseDrawerContract.DrawerActivity {

    private ProgressDialog progressDialog;

    private RevealJsonFormUtils jsonFormUtils;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        drawerView = new DrawerMenuView(this);
        progressDialog = new ProgressDialog(getContext());
        progressDialog.setCancelable(false);

    }

    @Override
    protected int getLayout() {
        return R.layout.fragment_event_register;
    }

    @Override
    protected void initializePresenter() {
        presenter = new EventRegisterFragmentPresenter(this, Constants.EventsRegister.VIEW_IDENTIFIER);
    }

    @Override
    public void initializeAdapter(Set<org.smartregister.configurableviews.model.View> visibleColumns) {
        EventViewHolder eventViewHolder = new EventViewHolder(getContext(), registerActionHandler, paginationViewHandler);
        clientAdapter = new RecyclerViewPaginatedAdapter(null, eventViewHolder, context().commonrepository(this.tablename));
        clientAdapter.setCurrentlimit(20);
        clientsView.setAdapter(clientAdapter);
    }

    @Override
    public void startMapActivity() {
        Intent intent = new Intent(getContext(), ListTasksActivity.class);
        getActivity().setResult(RESULT_OK, intent);
        getActivity().finish();
    }


    @Override
    public void setupViews(View view) {
        super.setupViews(view);
        view.findViewById(R.id.txt_map_label).setOnClickListener(v -> getPresenter().onOpenMapClicked());

        drawerView.initializeDrawerLayout();
        view.findViewById(R.id.drawerMenu).setOnClickListener(v -> drawerView.openDrawerLayout());
        drawerView.onResume();

        getSearchView().setHint(R.string.search);

        TextView filterTextView = view.findViewById(R.id.filter_text_view);
        filterTextView.setOnClickListener(v -> {
            getPresenter().onFilterTasksClicked();
        });

        if(!Country.KENYA.equals(BuildConfig.BUILD_COUNTRY) && !Country.RWANDA.equals(BuildConfig.BUILD_COUNTRY)){
            view.findViewById(R.id.data_collection_date_header).setVisibility(View.GONE);
        }
    }


    @Override
    public void setUniqueID(String s) {//not used
    }


    @Override
    public void setAdvancedSearchFormData(HashMap<String, String> hashMap) {//not used
    }

    @Override
    protected String getMainCondition() {
        return getPresenter().getMainCondition();
    }

    @Override
    protected String getDefaultSortQuery() {
        return getPresenter().getSortQuery();
    }

    @Override
    protected void startRegistration() {//not used
    }

    @Override
    protected void onViewClicked(View view) {
        EventRegisterDetails details = (EventRegisterDetails) view.getTag(R.id.patient_column);
        getPresenter().onEventSelected(details);
    }

    @Override
    public void showNotFoundPopup(String s) {//not used
    }

    private EventRegisterContract.Presenter getPresenter() {
        return (EventRegisterContract.Presenter) presenter;
    }

    @Override
    public void onDrawerClosed() {//Do nothing
    }

    @Override
    public RevealJsonFormUtils getJsonFormUtils() {
        return jsonFormUtils;
    }

    public void setJsonFormUtils(RevealJsonFormUtils jsonFormUtils) {
        this.jsonFormUtils = jsonFormUtils;
    }

    @Override
    public void startForm(JSONObject formName) {
        ((EventRegisterActivity) getActivity()).startFormActivity(formName);
    }

    @Override
    public void displayError(int title, int message) {
        new AlertDialog.Builder(getActivity()).setTitle(title).setMessage(message).create().show();
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
    public void openFilterActivity(TaskFilterParams filterParams) {
        Intent intent = new Intent(getContext(), FilterTasksActivity.class);
        intent.putExtra(FILTER_SORT_PARAMS, filterParams);
        List<String> forms = new ArrayList<>();
        forms.add(Constants.SPRAY_EVENT);
        forms.addAll(EventType.SUMMARY_EVENT_TYPES);
        forms.add(EventType.IRS_LITE_VERIFICATION);
        intent.putExtra(FILTER_CONFIGURATION, FilterConfiguration.builder()
                .businessStatusLayoutEnabled(true)
                .businessStatusList(Arrays.asList(BusinessStatus.COMPLETE, BusinessStatus.SPRAYED, BusinessStatus.NOT_SPRAYED, BusinessStatus.NOT_ELIGIBLE))
                .interventionTypeLayoutEnabled(false)
                .taskCodeLayoutEnabled(false)
                .formsLayoutEnabled(true)
                .filterFromDateAndViewAllEventsEnabled(true)
                .eventTypeList(forms)
                .sortOptions(R.array.form_sort_options)
                .build());
        getActivity().startActivityForResult(intent, REQUEST_CODE_FILTER_TASKS);

    }


    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == REQUEST_CODE_FILTER_TASKS && resultCode == RESULT_OK && data.hasExtra(FILTER_SORT_PARAMS)) {
            TaskFilterParams filterParams = (TaskFilterParams) data.getSerializableExtra(FILTER_SORT_PARAMS);
            getPresenter().filterTasks(filterParams);
        }
    }


}
