package org.smartregister.reveal.presenter;

import org.smartregister.domain.Task.TaskStatus;
import org.smartregister.reveal.fragment.TaskRegisterFragment;
import org.smartregister.reveal.util.Utils;
import org.smartregister.view.contract.BaseRegisterFragmentContract;

import java.lang.ref.WeakReference;
import java.util.HashSet;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterFragmentPresenter implements BaseRegisterFragmentContract.Presenter {

    private WeakReference<TaskRegisterFragment> view;

    public TaskRegisterFragmentPresenter(TaskRegisterFragment view) {
        this.view = new WeakReference<>(view);
    }

    @Override
    public void processViewConfigurations() {
    }

    @Override
    public void initializeQueries(String mainCondition) {
        getView().initializeAdapter(new HashSet<>());
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
