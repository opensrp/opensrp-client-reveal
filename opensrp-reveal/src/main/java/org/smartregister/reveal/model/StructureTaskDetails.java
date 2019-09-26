package org.smartregister.reveal.model;

import android.support.annotation.NonNull;

/**
 * Created by samuelgithengi on 4/11/19.
 */
public class StructureTaskDetails extends BaseTaskDetails {

    private String taskName;

    private String taskAction;

    private boolean edit;

    public StructureTaskDetails(@NonNull String taskId) {
        super(taskId);
    }

    public String getTaskName() {
        return taskName;
    }

    public void setTaskName(String taskName) {
        this.taskName = taskName;
    }

    public String getTaskAction() {
        return taskAction;
    }

    public void setTaskAction(String taskAction) {
        this.taskAction = taskAction;
    }

    public boolean isEdit() {
        return edit;
    }

    public void setEdit(boolean edit) {
        this.edit = edit;
    }
}
