package org.smartregister.reveal.model;

import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class TaskDetailsTest {

    @Test
    public void testSetGroupedTaskCodeStatusForAllTasks() {

        String groupedTaskCodeStatusString = "RACD Register Family-Complete,Blood Screening-Complete,Bednet Distribution-Complete";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isBednetDistributed());
        assertFalse(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isBednetDistributed());
        assertTrue(taskDetails.isFamilyRegistered());
        assertTrue(taskDetails.isBloodScreeningDone());
    }

    @Test
    public void testSetGroupedTaskCodeStatusForFamilyRegistration() {

        String groupedTaskCodeStatusString = "RACD Register Family-Complete,Blood Screening-In Progress,Bednet Distribution-In progress";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isBednetDistributed());
        assertFalse(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertFalse(taskDetails.isBednetDistributed());
        assertTrue(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());
    }

    @Test
    public void testSetGroupedTaskCodeStatusForBednetDistribution() {

        String groupedTaskCodeStatusString = "RACD Register Family-In Progress,Blood Screening-In Progress,Bednet Distribution-Complete";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isBednetDistributed());
        assertFalse(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isBednetDistributed());
        assertFalse(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());
    }

    @Test
    public void testSetGroupedTaskCodeStatusForBloodScreening() {

        String groupedTaskCodeStatusString = "RACD Register Family-In Progress,Blood Screening-Complete,Bednet Distribution-In Progress";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isBednetDistributed());
        assertFalse(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertFalse(taskDetails.isBednetDistributed());
        assertFalse(taskDetails.isFamilyRegistered());
        assertTrue(taskDetails.isBloodScreeningDone());
    }

}
