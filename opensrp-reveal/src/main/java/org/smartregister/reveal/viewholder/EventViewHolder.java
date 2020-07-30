package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.database.Cursor;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.recyclerview.widget.RecyclerView;

import org.joda.time.DateTime;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.cursoradapter.RecyclerViewProvider;
import org.smartregister.reveal.R;
import org.smartregister.reveal.util.Constants;
import org.smartregister.util.Utils;
import org.smartregister.view.contract.SmartRegisterClient;
import org.smartregister.view.contract.SmartRegisterClients;
import org.smartregister.view.dialog.FilterOption;
import org.smartregister.view.dialog.ServiceModeOption;
import org.smartregister.view.dialog.SortOption;
import org.smartregister.view.viewholder.OnClickFormLauncher;

import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.Locale;

/**
 * Created by samuelgithengi on 7/30/20.
 */
public class EventViewHolder implements RecyclerViewProvider<EventViewHolder.RegisterViewHolder> {


    private static String dateFormat = "dd-M-YYYY";
    private final Context context;
    private final View.OnClickListener registerClickListener;
    private final View.OnClickListener paginationClickListener;

    public EventViewHolder(Context context, View.OnClickListener registerClickListener, View.OnClickListener paginationClickListener) {
        this.context = context;
        this.registerClickListener = registerClickListener;
        this.paginationClickListener = paginationClickListener;
    }

    @Override
    public void getView(Cursor cursor, SmartRegisterClient smartRegisterClient, RegisterViewHolder registerViewHolder) {
        CommonPersonObjectClient pc = (CommonPersonObjectClient) smartRegisterClient;
        DateTime eventDate = DateTime.parse(Utils.getValue(pc.getColumnmaps(), Constants.DatabaseKeys.EVENT_DATE, false));
        registerViewHolder.eventDateTextView.setText(eventDate.toString(dateFormat));
        registerViewHolder.eventTypeTextView.setText(Utils.getValue(pc.getColumnmaps(), Constants.DatabaseKeys.EVENT_TYPE, false));
        registerViewHolder.sopTextView.setText(Utils.getValue(pc.getColumnmaps(), Constants.DatabaseKeys.SOP, false));
        registerViewHolder.householdTextView.setText(Utils.getValue(pc.getColumnmaps(), Constants.DatabaseKeys.ENTITY, false));
        registerViewHolder.statusTextView.setText(Utils.getValue(pc.getColumnmaps(), Constants.DatabaseKeys.STATUS, false));
    }

    @Override
    public void getFooterView(RecyclerView.ViewHolder viewHolder, int currentPageCount, int totalPageCount, boolean hasNext, boolean hasPrevious) {
        FooterViewHolder footerViewHolder = (FooterViewHolder) viewHolder;
        footerViewHolder.pageInfoView.setText(
                MessageFormat.format(context.getString(R.string.str_page_info), currentPageCount,
                        totalPageCount));

        footerViewHolder.nextPageView.setVisibility(hasNext ? View.VISIBLE : View.INVISIBLE);
        footerViewHolder.previousPageView.setVisibility(hasPrevious ? View.VISIBLE : View.INVISIBLE);

        footerViewHolder.nextPageView.setOnClickListener(paginationClickListener);
        footerViewHolder.previousPageView.setOnClickListener(paginationClickListener);
    }

    @Override
    public SmartRegisterClients updateClients(FilterOption villageFilter, ServiceModeOption serviceModeOption, FilterOption searchFilter, SortOption sortOption) {
        return null;
    }

    @Override
    public void onServiceModeSelected(ServiceModeOption serviceModeOption) {//not used
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
    public RegisterViewHolder createViewHolder(ViewGroup parent) {
        View view = inflater().inflate(R.layout.event_register_list_row, parent, false);
        return new RegisterViewHolder(view);
    }

    @Override
    public RecyclerView.ViewHolder createFooterHolder(ViewGroup parent) {
        View view = inflater().inflate(R.layout.smart_register_pagination, parent, false);
        return new FooterViewHolder(view);
    }

    @Override
    public boolean isFooterViewHolder(RecyclerView.ViewHolder viewHolder) {
        return FooterViewHolder.class.isInstance(viewHolder);
    }

    public class RegisterViewHolder extends RecyclerView.ViewHolder {

        private TextView eventDateTextView;
        private TextView eventTypeTextView;
        private TextView sopTextView;
        private TextView householdTextView;
        private TextView statusTextView;

        public RegisterViewHolder(View itemView) {
            super(itemView);
            eventDateTextView = itemView.findViewById(R.id.event_date);
            eventTypeTextView = itemView.findViewById(R.id.event_type);
            sopTextView = itemView.findViewById(R.id.sop);
            householdTextView = itemView.findViewById(R.id.entity);
            statusTextView = itemView.findViewById(R.id.status);
        }
    }
}
