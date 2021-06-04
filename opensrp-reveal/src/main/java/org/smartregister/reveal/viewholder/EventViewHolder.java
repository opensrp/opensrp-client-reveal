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
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.EventRegisterDetails;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.util.Country;
import org.smartregister.util.Utils;
import org.smartregister.view.contract.SmartRegisterClient;
import org.smartregister.view.contract.SmartRegisterClients;
import org.smartregister.view.dialog.FilterOption;
import org.smartregister.view.dialog.ServiceModeOption;
import org.smartregister.view.dialog.SortOption;
import org.smartregister.view.viewholder.OnClickFormLauncher;

import java.text.MessageFormat;
import java.util.Map;

/**
 * Created by samuelgithengi on 7/30/20.
 */
public class EventViewHolder implements RecyclerViewProvider<EventViewHolder.RegisterViewHolder> {

    public static final String CDD_SUPERVISOR_DAILY_SUMMARY = "Cdd Supervisor Daily Summary";
    public static final String CELL_COORDINATOR_DAILY_SUMMARY = "Cell Coordinator Daily Summary";
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
        EventRegisterDetails eventRegisterDetails = populateEventRegisterDetails(pc);
        DateTime eventDate = DateTime.parse(Utils.getValue(pc.getColumnmaps(), DatabaseKeys.EVENT_DATE, false));
        registerViewHolder.eventDateTextView.setText(eventDate.toString("dd-M-YYYY"));
        String eventType = Utils.getValue(pc.getColumnmaps(), DatabaseKeys.EVENT_TYPE, true);

        if(CDD_SUPERVISOR_DAILY_SUMMARY.equals(eventType) || CELL_COORDINATOR_DAILY_SUMMARY.equals(eventType)){
            String dataCollectionDate = Utils.getValue(pc.getColumnmaps(),DatabaseKeys.DATA_COLLECTION_DATE,false);
            registerViewHolder.dataCollectionDateTextView.setText(dataCollectionDate);
        }

        if (eventType.equalsIgnoreCase(Constants.SPRAY_EVENT)) {
            eventType = context.getString(R.string.hh_form);
        }
        registerViewHolder.eventTypeTextView.setText(eventType);
        String sop = Utils.getValue(pc.getColumnmaps(), DatabaseKeys.SOP, false);
        registerViewHolder.sopTextView.setText(sop.contains("-") ? sop.substring(sop.lastIndexOf("-") + 1) : sop);
        registerViewHolder.householdTextView.setText(Utils.getValue(pc.getColumnmaps(), DatabaseKeys.ENTITY, false));
        registerViewHolder.statusTextView.setText(getStatus(Utils.getValue(pc.getColumnmaps(), DatabaseKeys.EVENT_TYPE, false)
                , Utils.getValue(pc.getColumnmaps(), DatabaseKeys.STATUS, false), pc.getColumnmaps()));
        setClickHandler(registerClickListener, eventRegisterDetails, registerViewHolder.itemView);
    }

    private String getStatus(String eventType, String status, Map<String, String> columnMaps) {
        switch (eventType) {
            case Constants.EventType.DAILY_SUMMARY_EVENT:
                int found = Integer.parseInt(columnMaps.getOrDefault(DatabaseKeys.FOUND, "0"));
                int sprayed = Integer.parseInt(columnMaps.getOrDefault(DatabaseKeys.SPRAYED, "0"));
                return context.getString(R.string.daily_summary_status, found, sprayed, found - sprayed);
            case Constants.SPRAY_EVENT:
                return context.getString(R.string.complete).equalsIgnoreCase(status)
                        || context.getString(R.string.partially_sprayed).equalsIgnoreCase(status) ? context.getString(R.string.sprayed) : status;
            default:
                return status;
        }
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
        return viewHolder instanceof FooterViewHolder;
    }

    public static class RegisterViewHolder extends RecyclerView.ViewHolder {

        protected TextView eventDateTextView;
        protected TextView eventTypeTextView;
        protected TextView sopTextView;
        protected TextView householdTextView;
        protected TextView statusTextView;
        protected TextView dataCollectionDateTextView;

        public RegisterViewHolder(View itemView) {
            super(itemView);
            eventDateTextView = itemView.findViewById(R.id.event_date);
            eventTypeTextView = itemView.findViewById(R.id.event_type);
            sopTextView = itemView.findViewById(R.id.sop);
            householdTextView = itemView.findViewById(R.id.entity);
            statusTextView = itemView.findViewById(R.id.status);
            dataCollectionDateTextView = itemView.findViewById(R.id.data_collection_date);

            if(!Country.KENYA.equals(BuildConfig.BUILD_COUNTRY) && !Country.RWANDA.equals(BuildConfig.BUILD_COUNTRY)){
                dataCollectionDateTextView.setVisibility(View.GONE);
            }
        }
    }

    private void setClickHandler(View.OnClickListener onClickListener, EventRegisterDetails registerDetails, View view) {
        view.setOnClickListener(onClickListener);
        view.setTag(R.id.patient_column, registerDetails);
    }

    private EventRegisterDetails populateEventRegisterDetails(CommonPersonObjectClient pc) {
        EventRegisterDetails eventRegisterDetails = new EventRegisterDetails();
        eventRegisterDetails.setEventType(Utils.getValue(pc.getColumnmaps(), DatabaseKeys.EVENT_TYPE, false));
        boolean isSpray = Constants.SPRAY_EVENT.equals(eventRegisterDetails.getEventType());
        eventRegisterDetails.setFormSubmissionId(Utils.getValue(pc.getColumnmaps(), isSpray ? DatabaseKeys.FORM_SUBMISSION_ID : DatabaseKeys.BASE_ENTITY_ID, false));
        return eventRegisterDetails;
    }
}
