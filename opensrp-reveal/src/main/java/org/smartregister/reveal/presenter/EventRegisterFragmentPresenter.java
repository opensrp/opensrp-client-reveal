package org.smartregister.reveal.presenter;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.LocalDate;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.configurableviews.helper.ConfigurableViewsHelper;
import org.smartregister.configurableviews.model.View;
import org.smartregister.configurableviews.model.ViewConfiguration;
import org.smartregister.cursoradapter.SmartRegisterQueryBuilder;
import org.smartregister.domain.Event;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.EventRegisterContract;
import org.smartregister.reveal.interactor.EventRegisterFragmentInteractor;
import org.smartregister.reveal.model.EventRegisterDetails;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.Utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DETAILS;
import static org.smartregister.reveal.util.Constants.ENTITY_ID;

/**
 * Created by samuelgithengi on 7/30/20.
 */
public class EventRegisterFragmentPresenter implements EventRegisterContract.Presenter {

    private String viewConfigurationIdentifier;

    private ConfigurableViewsHelper viewsHelper;

    private Set<View> visibleColumns;

    private EventRegisterContract.View view;

    private EventRegisterFragmentInteractor interactor;

    private EventRegisterDetails eventRegisterDetails;

    private AllSharedPreferences allSharedPreferences;

    private TaskFilterParams filterParams;

    public EventRegisterFragmentPresenter(EventRegisterContract.View view, String viewConfigurationIdentifier) {
        this.view = view;
        this.interactor = new EventRegisterFragmentInteractor(this);
        this.viewConfigurationIdentifier = viewConfigurationIdentifier;
        this.viewsHelper = ConfigurableViewsLibrary.getInstance().getConfigurableViewsHelper();
        this.allSharedPreferences = RevealApplication.getInstance().getContext().allSharedPreferences();
    }

    @Override
    public void processViewConfigurations() {
        if (!StringUtils.isBlank(this.viewConfigurationIdentifier)) {
            ViewConfiguration viewConfiguration = viewsHelper.getViewConfiguration(this.viewConfigurationIdentifier);
            if (viewConfiguration != null) {
                visibleColumns = viewsHelper.getRegisterActiveColumns(this.viewConfigurationIdentifier);
            }
        }
    }

    @Override
    public void initializeQueries(String mainCondition) {

        String tableName = Constants.EventsRegister.TABLE_NAME;

        String countSelect = countSelect(tableName, mainCondition);
        String mainSelect = mainSelect(tableName, mainCondition);

        view.initializeQueryParams(tableName, countSelect, mainSelect);
        view.initializeAdapter(visibleColumns);

        view.countExecute();
        view.filterandSortInInitializeQueries();

    }

    private String mainSelect(String tableName, String mainCondition) {
        SmartRegisterQueryBuilder queryBUilder = new SmartRegisterQueryBuilder();
        if(Country.KENYA.equals(BuildConfig.BUILD_COUNTRY) || Country.RWANDA.equals(BuildConfig.BUILD_COUNTRY)) {
            List<String> mainColumns =  new ArrayList<>(Arrays.asList(mainColumns(tableName)));
            mainColumns.add(tableName + "." + DatabaseKeys.DATA_COLLECTION_DATE);
            queryBUilder.selectInitiateMainTable(tableName, mainColumns.toArray(new String[mainColumns.size()]));
        }else{
            queryBUilder.selectInitiateMainTable(tableName, mainColumns(tableName));
        }
        return queryBUilder.mainCondition(mainCondition);
    }

    protected String[] mainColumns(String tableName) {
        String[] columns = new String[]{
                tableName + ".relationalid",
                tableName + "." + DatabaseKeys.EVENT_DATE,
                tableName + "." + DatabaseKeys.EVENT_TYPE,
                tableName + "." + DatabaseKeys.SOP,
                tableName + "." + DatabaseKeys.ENTITY,
                tableName + "." + DatabaseKeys.STATUS,
                tableName + "." + DatabaseKeys.FORM_SUBMISSION_ID,
                tableName + "." + DatabaseKeys.BASE_ENTITY_ID,
                tableName + "." + DatabaseKeys.SPRAYED,
                tableName + "." + DatabaseKeys.FOUND
        };
        return columns;
    }

    private String countSelect(String tableName, String mainCondition) {
        SmartRegisterQueryBuilder countQueryBuilder = new SmartRegisterQueryBuilder();
        countQueryBuilder.selectInitiateMainTableCounts(tableName);
        return countQueryBuilder.mainCondition(mainCondition);
    }

    @Override
    public void startSync() {
        Utils.startImmediateSync();
    }

    @Override
    public void searchGlobally(String s) {
        // do nothing
    }

    @Override
    public void onEventFound(Event event) {
        String formName = view.getJsonFormUtils().getFormName(this.eventRegisterDetails.getEventType(), null);
        if (StringUtils.isBlank(formName)) {
            view.displayError(R.string.opening_form_title, R.string.form_not_found);
        } else {
            JSONObject formJSON = view.getJsonFormUtils().getFormJSON(view.getContext(), formName, null, null);
            view.getJsonFormUtils().populateForm(event, formJSON);
            view.getJsonFormUtils().populateFormWithServerOptions(formName, formJSON,null);
            try {
                formJSON.put(ENTITY_ID, event.getBaseEntityId());
                formJSON.put(DETAILS, new JSONObject(event.getDetails()));
            } catch (JSONException e) {
                Timber.e(e);
            }
            view.startForm(formJSON);
        }
        view.hideProgressDialog();
    }

    @Override
    public void onOpenMapClicked() {
        view.startMapActivity();
    }

    public void onEventSelected(EventRegisterDetails details) {
        view.showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
        this.eventRegisterDetails = details;
        interactor.findEvent(details.getFormSubmissionId());
    }

    @Override
    public void onFilterTasksClicked() {
        view.openFilterActivity(filterParams);
    }

    @Override
    public void filterTasks(TaskFilterParams filterParams) {
        this.filterParams = filterParams;
        initializeQueries(getMainCondition());
    }

    @Override
    public String getMainCondition() {
        StringBuilder stringBuilder = new StringBuilder();
        if (filterParams == null || !filterParams.isViewAllEvents()) {
            stringBuilder.append(String.format("%s = '%s'", DatabaseKeys.PROVIDER_ID, allSharedPreferences.fetchRegisteredANM()));
            stringBuilder.append(" AND ");
        }
        if (filterParams != null) {
            Set<String> forms = filterParams.getCheckedFilters().get(Constants.Filter.FORM_NAME);
            if (forms != null) {
                stringBuilder.append(String.format("%s IN ('%s')", DatabaseKeys.EVENT_TYPE, StringUtils.join(forms, "','")));
                stringBuilder.append(" AND ");
            }
            Set<String> status = filterParams.getCheckedFilters().get(Constants.Filter.STATUS);
            if (status != null) {
                if (status.contains(BusinessStatus.SPRAYED)) {
                    status.add(BusinessStatus.COMPLETE);
                    status.add(BusinessStatus.PARTIALLY_SPRAYED);
                }
                stringBuilder.append(String.format("%s IN ('%s')", DatabaseKeys.STATUS, StringUtils.join(status, "','")));
                stringBuilder.append(" AND ");
            }

            if (filterParams.getFromDate() != null) {
                stringBuilder.append(String.format("%s >= '%s'", DatabaseKeys.EVENT_DATE, new LocalDate(filterParams.getFromDate().getTime()).toString()));
                stringBuilder.append(" AND ");
            }
        }

        return stringBuilder.length() == 0 ? "" : stringBuilder.substring(0, stringBuilder.length() - 5);
    }

    @Override
    public String getSortQuery() {
        StringBuilder stringBuilder = new StringBuilder();
        if (filterParams == null || StringUtils.isBlank(filterParams.getSortBy())) {
            stringBuilder.append(Constants.DatabaseKeys.EVENT_DATE + " DESC");
        } else {
            int sortType = Arrays.asList(view.getContext().getResources().getStringArray(R.array.form_sort_options)).indexOf(filterParams.getSortBy());
            if (sortType == 0) {//date default
                stringBuilder.append(Constants.DatabaseKeys.EVENT_DATE + " DESC");
            } else if (sortType == 1) {//form
                stringBuilder.append(DatabaseKeys.EVENT_TYPE);
            } else if (sortType == 2) {//SOP
                stringBuilder.append(DatabaseKeys.SOP);
            }
        }
        return stringBuilder.toString();
    }
}
