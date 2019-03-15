package org.smartregister.reveal.provider;

import android.content.Context;
import android.database.Cursor;
import android.location.Location;
import android.support.v4.app.FragmentActivity;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.cursoradapter.RecyclerViewProvider;
import org.smartregister.repository.StructureRepository;
import org.smartregister.reveal.R;
import org.smartregister.reveal.viewholder.TaskRegisterViewHolder;
import org.smartregister.view.contract.SmartRegisterClient;
import org.smartregister.view.contract.SmartRegisterClients;
import org.smartregister.view.dialog.FilterOption;
import org.smartregister.view.dialog.ServiceModeOption;
import org.smartregister.view.dialog.SortOption;
import org.smartregister.view.viewholder.OnClickFormLauncher;

import java.util.Set;

/**
 * Created by samuelgithengi on 3/12/19.
 */
public class TaskRegisterProvider implements RecyclerViewProvider<TaskRegisterViewHolder> {

    private Context context;

    private StructureRepository structureRepository;


    public TaskRegisterProvider(FragmentActivity activity, ) {
        context = activity;
    }

    @Override
    public void getView(Cursor cursor, SmartRegisterClient client, TaskRegisterViewHolder viewHolder) {

        viewHolder.setIcon(R.drawable.ic_bcc);
        viewHolder.setTaskName("Behaviour Change communication");
        //viewHolder.setDistanceFromStructure(16);
        viewHolder.setTaskAction("Record BCC");
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
        return (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    }

    @Override
    public TaskRegisterViewHolder createViewHolder(ViewGroup parent) {
        View view = inflater().inflate(R.layout.task_register_row, parent, false);
        return new TaskRegisterViewHolder(view);
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
