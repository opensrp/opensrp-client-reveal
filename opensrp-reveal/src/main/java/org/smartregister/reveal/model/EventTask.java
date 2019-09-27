package org.smartregister.reveal.model;

public class EventTask {

    private int eventsPerTask;

    private String lastEventDate;

    private String taskId;

    public int getEventsPerTask() {
        return eventsPerTask;
    }

    public void setEventsPerTask(int eventsPerTask) {
        this.eventsPerTask = eventsPerTask;
    }

    public String getLastEventDate() {
        return lastEventDate;
    }

    public void setLastEventDate(String lastEventDate) {
        this.lastEventDate = lastEventDate;
    }

    public String getTaskId() {
        return taskId;
    }

    public void setTaskId(String taskId) {
        this.taskId = taskId;
    }
}
