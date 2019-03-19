package org.smartregister.reveal.provider;

import android.content.Context;
import android.database.Cursor;
import android.support.v4.app.FragmentActivity;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;

import org.smartregister.cursoradapter.RecyclerViewProvider;
import org.smartregister.domain.Task;
import org.smartregister.repository.StructureRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.viewholder.TaskRegisterViewHolder;
import org.smartregister.view.contract.SmartRegisterClient;
import org.smartregister.view.contract.SmartRegisterClients;
import org.smartregister.view.dialog.FilterOption;
import org.smartregister.view.dialog.ServiceModeOption;
import org.smartregister.view.dialog.SortOption;
import org.smartregister.view.viewholder.OnClickFormLauncher;

import java.text.MessageFormat;
import java.util.Random;

/**
 * Created by samuelgithengi on 3/12/19.
 */
public class TaskRegisterProvider implements RecyclerViewProvider<TaskRegisterViewHolder> {

    private Context context;
    private View.OnClickListener registerActionHandler;
    private View.OnClickListener paginationClickListener;

    private StructureRepository structureRepository;

    private TaskRepository taskRepository;

    public TaskRegisterProvider(FragmentActivity activity, View.OnClickListener registerActionHandler, View.OnClickListener paginationClickListener) {
        context = activity;
        this.registerActionHandler = registerActionHandler;
        this.paginationClickListener = paginationClickListener;
        taskRepository = RevealApplication.getInstance().getTaskRepository();
    }

    @Override
    public void getView(Cursor cursor, SmartRegisterClient client, TaskRegisterViewHolder viewHolder) {
        Random random = new Random();
        Task task = taskRepository.readNativeCursor(cursor);
        Float distance = 16f;
        String name = cursor.getString(cursor.getColumnIndex(DatabaseKeys.FAMILY_NAME));
        String action = null;
        if (Constants.Intervention.IRS.equals(task.getCode())) {
            if (name == null) {
                name = "Structure " + random.nextInt(100);
            }
            action = context.getString(R.string.record_status);
        } else if (Constants.Intervention.MOSQUITO_COLLECTION.equals(task.getCode())) {
            name = context.getString(R.string.mosquito_collection_point);
            action = context.getString(R.string.record_mosquito_collection);
        } else if (Constants.Intervention.LARVAL_DIPPING.equals(task.getCode())) {
            name = context.getString(R.string.larval_breeding_site);
            action = context.getString(R.string.record_larvacide);
        } else if (Constants.Intervention.BCC.equals(task.getCode())) {
            viewHolder.setIcon(R.drawable.ic_bcc);
            name = context.getString(R.string.bcc);
            action = context.getString(R.string.record_bcc);
            distance = null;
        }
        viewHolder.setTaskName(name);
        viewHolder.setTaskAction(action, registerActionHandler);
        if (distance != null)
            viewHolder.setDistanceFromStructure(distance);
    }

    @Override
    public void getFooterView(RecyclerView.ViewHolder viewHolder, int currentPageCount, int totalPageCount, boolean hasNextPage, boolean hasPreviousPage) {
        FooterViewHolder footerViewHolder = (FooterViewHolder) viewHolder;
        footerViewHolder.pageInfoView.setText(
                MessageFormat.format(context.getString(R.string.str_page_info), currentPageCount,
                        totalPageCount));

        footerViewHolder.nextPageView.setVisibility(hasNextPage ? View.VISIBLE : View.INVISIBLE);
        footerViewHolder.previousPageView.setVisibility(hasPreviousPage ? View.VISIBLE : View.INVISIBLE);

        footerViewHolder.nextPageView.setOnClickListener(paginationClickListener);
        footerViewHolder.previousPageView.setOnClickListener(paginationClickListener);
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
        View view = inflater().inflate(R.layout.smart_register_pagination, parent, false);
        return new FooterViewHolder(view);
    }

    @Override
    public boolean isFooterViewHolder(RecyclerView.ViewHolder viewHolder) {
        return viewHolder instanceof FooterViewHolder;
    }

    private class FooterViewHolder extends RecyclerView.ViewHolder {
        private TextView pageInfoView;
        private Button nextPageView;
        private Button previousPageView;

        private FooterViewHolder(View view) {
            super(view);

            nextPageView = view.findViewById(R.id.btn_next_page);
            previousPageView = view.findViewById(R.id.btn_previous_page);
            pageInfoView = view.findViewById(R.id.txt_page_info);
        }
    }

}
