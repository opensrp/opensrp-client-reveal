package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import org.smartregister.cursoradapter.RecyclerViewPaginatedAdapter;
import org.smartregister.family.fragment.NoMatchDialogFragment;
import org.smartregister.reveal.R;
import org.smartregister.reveal.presenter.TaskRegisterFragmentPresenter;
import org.smartregister.reveal.provider.TaskRegisterProvider;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.TaskRegister;
import org.smartregister.view.activity.BaseRegisterActivity;
import org.smartregister.view.contract.BaseRegisterFragmentContract;
import org.smartregister.view.fragment.BaseRegisterFragment;

import java.util.HashMap;
import java.util.Set;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterFragment extends BaseRegisterFragment implements BaseRegisterFragmentContract.View {


    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_task_register, container, false);
        rootView = view;//handle to the root

        Toolbar toolbar = view.findViewById(R.id.register_toolbar);
        AppCompatActivity activity = ((AppCompatActivity) getActivity());

        activity.setSupportActionBar(toolbar);
        activity.getSupportActionBar().setTitle(activity.getIntent().getStringExtra(TOOLBAR_TITLE));
        activity.getSupportActionBar().setDisplayHomeAsUpEnabled(false);

        activity.getSupportActionBar().setLogo(R.drawable.round_white_background);
        activity.getSupportActionBar().setDisplayUseLogoEnabled(false);
        activity.getSupportActionBar().setDisplayShowTitleEnabled(false);

        setupViews(view);
        return view;
    }

    public void initializeAdapter(Set<org.smartregister.configurableviews.model.View> visibleColumns) {
        TaskRegisterProvider taskRegisterProvider = new TaskRegisterProvider(getActivity(), commonRepository(), visibleColumns, registerActionHandler, paginationViewHandler);
        clientAdapter = new RecyclerViewPaginatedAdapter(null, taskRegisterProvider, context().commonrepository(this.tablename));
        clientAdapter.setCurrentlimit(20);
        clientsView.setAdapter(clientAdapter);
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
        return getPresenter().getMainCondition();
    }

    @Override
    protected String getDefaultSortQuery() {
        return getPresenter().getDefaultSortQuery();
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
}
