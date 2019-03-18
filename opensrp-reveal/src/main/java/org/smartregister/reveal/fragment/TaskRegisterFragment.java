package org.smartregister.reveal.fragment;

import android.database.Cursor;
import android.os.Bundle;
import android.support.v4.content.CursorLoader;
import android.support.v4.content.Loader;
import android.util.Log;
import android.view.View;
import android.widget.TextView;

import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.cursoradapter.RecyclerViewPaginatedAdapter;
import org.smartregister.cursoradapter.SmartRegisterQueryBuilder;
import org.smartregister.family.fragment.NoMatchDialogFragment;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.presenter.TaskRegisterFragmentPresenter;
import org.smartregister.reveal.provider.TaskRegisterProvider;
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

    private static final String COUNT = "count_execute";

    @Override
    protected int getLayout() {
        return R.layout.fragment_task_register;
    }

    public void initializeAdapter(Set<org.smartregister.configurableviews.model.View> visibleColumns) {
        TaskRegisterProvider taskRegisterProvider = new TaskRegisterProvider(getActivity(), registerActionHandler, paginationViewHandler);
        clientAdapter = new RecyclerViewPaginatedAdapter(null, taskRegisterProvider, context().commonrepository(this.tablename));
        clientAdapter.setCurrentlimit(20);
        clientAdapter.setQueryClients(false);
        clientsView.setAdapter(clientAdapter);
    }

    @Override
    public void setupViews(View view) {
        super.setupViews(view);
        if (getActivity() != null) {
            ((TextView) view.findViewById(R.id.intervention_type)).setText(
                    getActivity().getIntent().getStringExtra(TaskRegister.INTERVENTION_TYPE));
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


    @Override
    public Loader<Cursor> onCreateLoader(int id, final Bundle args) {
        switch (id) {
            case LOADER_ID:
                // Returns a new CursorLoader
                return new CursorLoader(getActivity()) {
                    @Override
                    public Cursor loadInBackground() {
                        // Count query
                        if (args != null && args.getBoolean(COUNT)) {
                            countExecute();
                        }
                        SmartRegisterQueryBuilder sqb = new SmartRegisterQueryBuilder(getPresenter().mainSelect());

                        String query = sqb.Endquery(sqb.addlimitandOffset(sqb.getSelectquery(), clientAdapter.getCurrentlimit(), clientAdapter.getCurrentoffset()));
                        // Select register query

                        return getRepository().rawQuery(query, null);
                    }
                };
            default:
                // An invalid id was passed in
                return null;
        }

    }


    public void countExecute() {
        Cursor c = null;

        try {
            String query = getPresenter().countSelect();

            Log.i(getClass().getName(), query);
            c = getRepository().rawQuery(query, null);
            c.moveToFirst();
            clientAdapter.setTotalcount(c.getInt(0));
            Log.v("total count here", "" + clientAdapter.getTotalcount());

            clientAdapter.setCurrentoffset(0);


        } catch (Exception e) {
            Log.e(getClass().getName(), e.toString(), e);
        } finally {
            if (c != null) {
                c.close();
            }
        }
    }

    private SQLiteDatabase getRepository() {
        return RevealApplication.getInstance().getRepository().getReadableDatabase();
    }

}
