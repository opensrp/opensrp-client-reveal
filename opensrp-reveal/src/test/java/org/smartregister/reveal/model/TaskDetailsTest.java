package org.smartregister.reveal.model;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.util.PreferencesUtil;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BEDNET_DISTRIBUTED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BLOOD_SCREENING_COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FAMILY_REGISTERED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FULLY_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NONE_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.PARTIALLY_RECEIVED;

@RunWith(PowerMockRunner.class)
@PrepareForTest({PreferencesUtil.class})
public class TaskDetailsTest extends BaseUnitTest {

    public PreferencesUtil preferencesUtil;

    @Before
    public void setup() throws Exception {
        PowerMockito.mockStatic(PreferencesUtil.class);
        preferencesUtil = mock(PreferencesUtil.class);
        PowerMockito.when(PreferencesUtil.class, "getInstance").thenReturn(preferencesUtil);
        when(preferencesUtil.getCurrentPlan()).thenReturn("FI");
        when(preferencesUtil.getInterventionTypeForPlan("FI")).thenReturn("FI");
    }

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
        assertEquals(COMPLETE, taskDetails.getAggregateBusinessStatus());
    }

    @Test
    public void testSetGroupedTaskCodeStatusForAllTasksWithOneInvalidEntry() {
        String groupedTaskCodeStatusString = "Invalid,RACD Register Family-Complete,Blood Screening-Complete,Bednet Distribution-Complete";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isBednetDistributed());
        assertFalse(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isBednetDistributed());
        assertTrue(taskDetails.isFamilyRegistered());
        assertTrue(taskDetails.isBloodScreeningDone());
        assertEquals(COMPLETE, taskDetails.getAggregateBusinessStatus());
    }

    @Test
    public void testSetGroupedTaskCodeStatusForEmpty() {
        String groupedTaskCodeStatusString = "";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isBednetDistributed());
        assertFalse(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);
        assertFalse(taskDetails.isBednetDistributed());
        assertFalse(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());
        assertFalse(taskDetails.isFamilyRegTaskExists());
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
        assertEquals(FAMILY_REGISTERED, taskDetails.getAggregateBusinessStatus());
    }

    @Test
    public void testSetGroupedTaskCodeStatusForCaseConfirmation() {

        String groupedTaskCodeStatusString = "RACD Register Family-Complete,Case Confirmation-Complete";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isBednetDistributed());
        assertFalse(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isFamilyRegistered());
        assertTrue(taskDetails.isBloodScreeningDone());
        assertEquals(BLOOD_SCREENING_COMPLETE, taskDetails.getAggregateBusinessStatus());
    }

    @Test
    public void testSetGroupedTaskCodeStatusForBednetDistribution() {

        String groupedTaskCodeStatusString = "RACD Register Family-Complete,Blood Screening-In Progress,Bednet Distribution-Complete";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isBednetDistributed());
        assertFalse(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isBednetDistributed());
        assertTrue(taskDetails.isFamilyRegistered());
        assertFalse(taskDetails.isBloodScreeningDone());
        assertEquals(BEDNET_DISTRIBUTED, taskDetails.getAggregateBusinessStatus());
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
        assertEquals(BLOOD_SCREENING_COMPLETE, taskDetails.getAggregateBusinessStatus());
    }

    @Test
    public void testSetGroupedTaskCodeStatusForAdherenceVisitDone() {
        when(preferencesUtil.getCurrentPlan()).thenReturn("MDA");
        when(preferencesUtil.getInterventionTypeForPlan("MDA")).thenReturn("MDA");
        String groupedTaskCodeStatusString = "MDA Dispense-Fully Received,MDA Adherence-Complete";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isMdaAdhered());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isMdaAdhered());
        assertEquals(COMPLETE, taskDetails.getAggregateBusinessStatus());

    }

    @Test
    public void testSetGroupedTaskCodeStatusForMDADispenseFullyReceived() {
        when(preferencesUtil.getCurrentPlan()).thenReturn("MDA");
        when(preferencesUtil.getInterventionTypeForPlan("MDA")).thenReturn("MDA");
        String groupedTaskCodeStatusString = "MDA Dispense-Fully Received,MDA Dispense-Fully Received";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isFullyReceived());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isFullyReceived());
        assertEquals(FULLY_RECEIVED, taskDetails.getAggregateBusinessStatus());
    }

    @Test
    public void testSetGroupedTaskCodeStatusForMDADispensePartiallyReceived() {
        when(preferencesUtil.getCurrentPlan()).thenReturn("MDA");
        when(preferencesUtil.getInterventionTypeForPlan("MDA")).thenReturn("MDA");
        String groupedTaskCodeStatusString = "MDA Dispense-Fully Received,MDA Dispense-None Received, MDA Dispense-Not Eligible";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isPartiallyReceived());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isPartiallyReceived());
        assertEquals(PARTIALLY_RECEIVED, taskDetails.getAggregateBusinessStatus());
    }

    @Test
    public void testSetGroupedTaskCodeStatusForMDADispenseNoneReceived() {
        when(preferencesUtil.getCurrentPlan()).thenReturn("MDA");
        when(preferencesUtil.getInterventionTypeForPlan("MDA")).thenReturn("MDA");
        String groupedTaskCodeStatusString = "MDA Dispense-None Received,MDA Dispense-None Received";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isNoneReceived());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isNoneReceived());
        assertEquals(NONE_RECEIVED, taskDetails.getAggregateBusinessStatus());

    }

    @Test
    public void testSetGroupedTaskCodeStatusForMDADispenseNotElligible() {
        when(preferencesUtil.getCurrentPlan()).thenReturn("MDA");
        when(preferencesUtil.getInterventionTypeForPlan("MDA")).thenReturn("MDA");
        String groupedTaskCodeStatusString = "MDA Dispense-Not Eligible,MDA Dispense-Not Eligible";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isNotEligible());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isNotEligible());
        assertEquals(FAMILY_REGISTERED, taskDetails.getAggregateBusinessStatus());

    }

    @Test
    public void testSetGroupedTaskCodeStatusForInElligible() {
        when(preferencesUtil.getCurrentPlan()).thenReturn("MDA");
        when(preferencesUtil.getInterventionTypeForPlan("MDA")).thenReturn("A");
        String groupedTaskCodeStatusString = "MDA Dispense-Not Eligible,MDA Dispense-Not Eligible";
        TaskDetails taskDetails = new TaskDetails("task1");

        assertFalse(taskDetails.isNotEligible());

        taskDetails.setGroupedTaskCodeStatus(groupedTaskCodeStatusString);

        assertTrue(taskDetails.isNotEligible());
        assertEquals(NOT_ELIGIBLE, taskDetails.getAggregateBusinessStatus());

    }

    @Test
    public void testGetCount() {
        TaskDetails taskDetails = new TaskDetails("task1");
        taskDetails.setCompleteTaskCount(1);
        assertEquals(1, (int) taskDetails.getCompleteTaskCount());
    }

    @Test
    public void testSetMDAAdhered() {
        TaskDetails taskDetails = new TaskDetails("task1");
        taskDetails.setMdaAdhered(true);
        assertTrue(taskDetails.isMdaAdhered());

        taskDetails.setMdaAdhered(false);
        assertFalse(taskDetails.isMdaAdhered());
    }

    @Test
    public void testGroupedTaskCode() {
        TaskDetails taskDetails = new TaskDetails("task1");
        String groupedTaskCodeStatusString = "RACD Register Family-Complete,Blood Screening-In Progress,Bednet Distribution-Complete";
        taskDetails.setGroupedTaskCodes(groupedTaskCodeStatusString);
        assertEquals("~RACD Register Family~Blood Screening~Bednet Distribution", taskDetails.getGroupedTaskCodes());
    }

}
