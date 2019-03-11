package org.smartregister.reveal.presenter;

import org.smartregister.domain.Task.TaskStatus;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterFragmentPresenter {


    public String getMainCondition() {
        return String.format(" status IN (%s, %s) ", TaskStatus.READY, TaskStatus.IN_PROGRESS);
    }

    public String getDefaultSortQuery() {
        return "start DESC ";
    }
}
