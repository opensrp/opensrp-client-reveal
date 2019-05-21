package org.smartregister.reveal.presenter;

import android.support.v4.util.Pair;

import com.google.common.annotations.VisibleForTesting;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.configurableviews.helper.ConfigurableViewsHelper;
import org.smartregister.configurableviews.model.View;
import org.smartregister.configurableviews.model.ViewConfiguration;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.TaskRegisterFragmentContract;
import org.smartregister.reveal.interactor.TaskRegisterFragmentInteractor;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.smartregister.reveal.util.Constants.Intervention.BCC;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterFragmentPresenter extends BaseFormFragmentPresenter implements TaskRegisterFragmentContract.Presenter {

    private WeakReference<TaskRegisterFragmentContract.View> view;

    private String viewConfigurationIdentifier;

    private ConfigurableViewsHelper viewsHelper;

    private Set<View> visibleColumns;

    private TaskRegisterFragmentInteractor interactor;

    private List<TaskDetails> tasks;
    private android.location.Location lastLocation;

    private boolean recalculateDistance;

    private PreferencesUtil prefsUtil;


    public TaskRegisterFragmentPresenter(TaskRegisterFragmentContract.View view, String viewConfigurationIdentifier) {
        this(view, viewConfigurationIdentifier, null);
        this.interactor = new TaskRegisterFragmentInteractor(this);
    }

    @VisibleForTesting
    protected TaskRegisterFragmentPresenter(TaskRegisterFragmentContract.View view, String viewConfigurationIdentifier,
                                            TaskRegisterFragmentInteractor interactor) {
        super(view, view.getContext());
        this.view = new WeakReference<>(view);
        this.viewConfigurationIdentifier = viewConfigurationIdentifier;
        this.interactor = interactor;
        viewsHelper = ConfigurableViewsLibrary.getInstance().getConfigurableViewsHelper();
        prefsUtil = PreferencesUtil.getInstance();


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

        getView().initializeAdapter(visibleColumns);
        lastLocation = getView().getLocationUtils().getLastLocation();
        if (lastLocation == null) {//if location client has not initialized use last location passed from map
            lastLocation = getView().getLastLocation();
        }

        getView().showProgressView();

        interactor.findTasks(getMainCondition(), lastLocation, getOperationalAreaCenter(), getView().getContext().getString(R.string.house));

    }

    private android.location.Location getOperationalAreaCenter() {
        Location operationalAreaLocation = Utils.getOperationalAreaLocation(prefsUtil.getCurrentOperationalArea());
        if (operationalAreaLocation == null)
            return null;
        return mappingHelper.getCenter(gson.toJson(operationalAreaLocation.getGeometry()));
    }

    @Override
    public void startSync() {
        Utils.startImmediateSync();
    }

    @Override
    public void searchGlobally(String uniqueId) {//do nothing, tasks not searchable globally
    }

    /**
     * Gets the where clause for the task register, filters by operational area and campaign
     *
     * @return pair of filter clause and values for filter
     */
    private Pair<String, String[]> getMainCondition() {
        Location operationalArea = Utils.getOperationalAreaLocation(prefsUtil.getCurrentOperationalArea());
        String whereClause = String.format("%s = ? AND %s = ?", DatabaseKeys.GROUPID, DatabaseKeys.PLAN_ID);
        return new Pair<>(whereClause, new String[]{operationalArea == null ?
                null : operationalArea.getId(), prefsUtil.getCurrentPlanId()});
    }

    @Override
    protected TaskRegisterFragmentContract.View getView() {
        return view.get();
    }

    @Override
    public void onTasksFound(List<TaskDetails> tasks, int structuresWithinBuffer) {
        if (recalculateDistance) {//there was a location update when tasks were being retrieved recalculate distance and order
            interactor.calculateDistanceFromUser(tasks, lastLocation);
            recalculateDistance = false;
        } else {
            this.tasks = tasks;
            if (tasks == null) {
                getView().displayNotification(R.string.fetching_structure_title,
                        R.string.fetch_location_and_structures_failed, prefsUtil.getCurrentOperationalArea());
                getView().setTaskDetails(new ArrayList<>());
            } else if (tasks.isEmpty()) {
                getView().displayNotification(R.string.fetching_structure_title, R.string.no_structures_found);
                getView().setTaskDetails(tasks);
            } else {
                getView().setTaskDetails(tasks);
            }
            getView().setTotalTasks(structuresWithinBuffer);
            getView().hideProgressDialog();
            getView().hideProgressView();
        }

    }


    @Override
    public void onLocationChanged(android.location.Location location) {
        if (!location.equals(lastLocation)) {
            if (lastLocation == null && tasks == null) {//tasks not yet retrieved from db
                recalculateDistance = true;
            } else if (lastLocation == null ||
                    location.distanceTo(lastLocation) >= Constants.REFRESH_MAP_MINIMUM_DISTANCE) {
                interactor.calculateDistanceFromUser(tasks, location);
            }
            lastLocation = location;
        }

    }

    @Override
    public void onDestroy() {
        getView().getLocationUtils().stopLocationClient();
    }

    @Override
    public void onDrawerClosed() {
        getView().showProgressDialog(R.string.fetching_structures_title, R.string.fetching_structures_message);
        interactor.findTasks(getMainCondition(), lastLocation, getOperationalAreaCenter(), getView().getContext().getString(R.string.house));
        getView().setInventionType(getInterventionLabel());
    }

    @Override
    public void onTaskSelected(TaskDetails details) {
        if (details != null) {
            if (Task.TaskStatus.COMPLETED.name().equals(details.getTaskStatus()) && !BCC.equals(details.getTaskCode())) {
                //TODO implement functionality to link to structure details once its implemented
                getView().displayToast(String.format("To open structure details view for %s",
                        details.getFamilyName()));
            } else if (CASE_CONFIRMATION.equals(details.getTaskCode()) ||
                    BLOOD_SCREENING.equals(details.getTaskCode()) ||
                    BEDNET_DISTRIBUTION.equals(details.getTaskCode())) {
                //TODO implement functionality to link to structure details once its implemented
                getView().displayToast(String.format("To open family view for %s",
                        details.getFamilyName()));
            } else {
                getView().showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
                interactor.getStructure(details);
            }
        }
    }

    @Override
    public int getInterventionLabel() {
        return Utils.getInterventionLabel();
    }
}
