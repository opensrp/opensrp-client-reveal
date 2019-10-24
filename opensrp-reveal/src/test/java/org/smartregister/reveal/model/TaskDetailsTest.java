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

    @Test
    public void testSetGroupedTaskCodeStatusForAdherenceVisitDone() {
        String groupedTaskCodeStatusString = "MDA Dispense-Fully Received,MDA Adherence-Complete";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isMdaAdhered());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isMdaAdhered());

    }

    @Test
    public void testSetGroupedTaskCodeStatusForMDADispenseFullyReceived() {
        String groupedTaskCodeStatusString = "MDA Dispense-Fully Received,MDA Dispense-Fully Received";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isFullyReceived());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isFullyReceived());

    }

    @Test
    public void testSetGroupedTaskCodeStatusForMDADispensePartiallyReceived() {
        String groupedTaskCodeStatusString = "MDA Dispense-Fully Received,MDA Dispense-None Received, MDA Dispense-Not Eligible";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isPartiallyReceived());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isPartiallyReceived());

    }

    @Test
    public void testSetGroupedTaskCodeStatusForMDADispenseNoneReceived() {
        String groupedTaskCodeStatusString = "MDA Dispense-None Received,MDA Dispense-None Received";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isNoneReceived());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isNoneReceived());

    }

    @Test
    public void testSetGroupedTaskCodeStatusForMDADispenseNotElligible() {
        String groupedTaskCodeStatusString = "MDA Dispense-Not Eligible,MDA Dispense-Not Eligible";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isNotEligible());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isNotEligible());

    }

}
