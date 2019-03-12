package org.smartregister.reveal.provider;

import android.database.Cursor;
import android.support.v4.app.FragmentActivity;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.configurableviews.model.View;
import org.smartregister.cursoradapter.RecyclerViewFragment;
import org.smartregister.cursoradapter.RecyclerViewProvider;
import org.smartregister.family.provider.FamilyRegisterProvider;
import org.smartregister.reveal.viewholder.TaskRegisterViewHolder;
import org.smartregister.view.contract.SmartRegisterClient;
import org.smartregister.view.contract.SmartRegisterClients;
import org.smartregister.view.dialog.FilterOption;
import org.smartregister.view.dialog.ServiceModeOption;
import org.smartregister.view.dialog.SortOption;
import org.smartregister.view.fragment.BaseRegisterFragment;
import org.smartregister.view.viewholder.OnClickFormLauncher;

import java.util.Set;

/**
 * Created by samuelgithengi on 3/12/19.
 */
public class TaskRegisterProvider implements RecyclerViewProvider<TaskRegisterViewHolder> {


    public TaskRegisterProvider(FragmentActivity activity, CommonRepository commonRepository, Set<View> visibleColumns, android.view.View.OnClickListener registerActionHandler, android.view.View.OnClickListener paginationViewHandler) {
    }

    @Override
    public void getView(Cursor cursor, SmartRegisterClient client, TaskRegisterViewHolder viewHolder) {

    }

    @Override
    public void getFooterView(RecyclerView.ViewHolder viewHolder, int currentPageCount, int totalCount, boolean hasNextPage, boolean hasPreviousPage) {

    }

    @Override
    public SmartRegisterClients updateClients(FilterOption villageFilter, ServiceModeOption serviceModeOption, FilterOption searchFilter, SortOption sortOption) {
        return null;
    }

    @Override
    public void onServiceModeSelected(ServiceModeOption serviceModeOption) {

    }

    @Override
    public OnClickFormLauncher newFormLauncher(String formName, String entityId, String metaData) {
        return null;
    }

    @Override
    public LayoutInflater inflater() {
        return null;
    }

    @Override
    public TaskRegisterViewHolder createViewHolder(ViewGroup parent) {
        return null;
    }

    @Override
    public RecyclerView.ViewHolder createFooterHolder(ViewGroup parent) {
        return null;
    }

    @Override
    public boolean isFooterViewHolder(RecyclerView.ViewHolder viewHolder) {
        return false;
    }
}
