package org.smartregister.reveal.model;

import androidx.annotation.NonNull;

import java.util.Date;

/**
 * Created by samuelgithengi on 4/11/19.
 */
public class StructureTaskDetails extends BaseTaskDetails {

    private String taskName;

    private String taskAction;

    private boolean edit;

    private Date lastEdited;

    private String personTested;

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

    public Date getLastEdited() {
        return lastEdited;
    }

    public void setLastEdited(Date lastEdited) {
        this.lastEdited = lastEdited;
    }

    public String getPersonTested() {
        return personTested;
    }

    public void setPersonTested(String personTested) {
        this.personTested = personTested;
    }
}
