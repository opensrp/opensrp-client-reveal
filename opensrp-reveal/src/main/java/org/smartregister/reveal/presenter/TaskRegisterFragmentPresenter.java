package org.smartregister.reveal.presenter;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.configurableviews.helper.ConfigurableViewsHelper;
import org.smartregister.configurableviews.model.RegisterConfiguration;
import org.smartregister.configurableviews.model.View;
import org.smartregister.configurableviews.model.ViewConfiguration;
import org.smartregister.domain.Task.TaskStatus;
import org.smartregister.reveal.fragment.TaskRegisterFragment;
import org.smartregister.reveal.util.Utils;
import org.smartregister.view.contract.BaseRegisterFragmentContract;

import java.lang.ref.WeakReference;
import java.util.Set;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterFragmentPresenter implements BaseRegisterFragmentContract.Presenter {

    private WeakReference<TaskRegisterFragment> view;

    private String viewConfigurationIdentifier;

    private ConfigurableViewsHelper viewsHelper;


    private Set<View> visibleColumns;


    public TaskRegisterFragmentPresenter(TaskRegisterFragment view, String viewConfigurationIdentifier) {
        this.view = new WeakReference<>(view);
        this.viewConfigurationIdentifier = viewConfigurationIdentifier;
        viewsHelper = ConfigurableViewsLibrary.getInstance().getConfigurableViewsHelper();
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
        return String.format(" status IN (%s, %s) ", TaskStatus.READY, TaskStatus.IN_PROGRESS);
    }

    public String getDefaultSortQuery() {
        return "start DESC ";
    }

    private TaskRegisterFragment getView() {
        return view.get();
    }

}
