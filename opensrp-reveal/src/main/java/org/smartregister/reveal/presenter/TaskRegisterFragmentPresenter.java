package org.smartregister.reveal.presenter;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.configurableviews.helper.ConfigurableViewsHelper;
import org.smartregister.configurableviews.model.RegisterConfiguration;
import org.smartregister.configurableviews.model.View;
import org.smartregister.configurableviews.model.ViewConfiguration;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task.TaskStatus;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.TaskRegisterFragmentContract;
import org.smartregister.reveal.fragment.TaskRegisterFragment;
import org.smartregister.reveal.interactor.TaskRegisterFragmentInteractor;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.Set;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterFragmentPresenter implements TaskRegisterFragmentContract.Presenter {

    private WeakReference<TaskRegisterFragment> view;

    private String viewConfigurationIdentifier;

    private ConfigurableViewsHelper viewsHelper;

    private Set<View> visibleColumns;

    private TaskRegisterFragmentInteractor interactor;

    public TaskRegisterFragmentPresenter(TaskRegisterFragment view, String viewConfigurationIdentifier) {
        this.view = new WeakReference<>(view);
        this.viewConfigurationIdentifier = viewConfigurationIdentifier;
        viewsHelper = ConfigurableViewsLibrary.getInstance().getConfigurableViewsHelper();
        interactor = new TaskRegisterFragmentInteractor(RevealApplication.getInstance().getAppExecutors(), this);
    }

    @Override
    public void processViewConfigurations() {
        if (!StringUtils.isBlank(this.viewConfigurationIdentifier)) {
            ViewConfiguration viewConfiguration = viewsHelper.getViewConfiguration(this.viewConfigurationIdentifier);
            if (viewConfiguration != null) {
                RegisterConfiguration config = (RegisterConfiguration) viewConfiguration.getMetadata();
                visibleColumns = viewsHelper.getRegisterActiveColumns(this.viewConfigurationIdentifier);
            }
        }
    }

    @Override
    public void initializeQueries(String mainCondition) {

        getView().initializeAdapter(visibleColumns);
        interactor.findTasks(getMainCondition());

        /*getView().countExecute();
        getView().filterandSortInInitializeQueries();*/
        getView().showProgressView();
    }

    @Override
    public void startSync() {
        Utils.startImmediateSync();
    }

    @Override
    public void searchGlobally(String uniqueId) {
        //do nothing, tasks not searchable globally
    }

    public String getMainCondition() {
        Location operationalArea = Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        return String.format(" group_id = '%s' AND status IN ('%s', '%s') ", operationalArea.getId(), TaskStatus.READY, TaskStatus.IN_PROGRESS);
    }

    public String getDefaultSortQuery() {
        return "start DESC ";
    }

    private TaskRegisterFragment getView() {
        return view.get();
    }

    @Override
    public void onTasksFound(List<TaskDetails> tasks) {
        getView().setTaskDetails(tasks);
        getView().setTotalPatients();
        getView().hideProgressView();
    }
}
