package org.smartregister.reveal.job;

import com.evernote.android.job.Job;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.smartregister.job.DocumentConfigurationServiceJob;
import org.smartregister.job.ExtendedSyncServiceJob;
import org.smartregister.job.P2pServiceJob;
import org.smartregister.job.PullUniqueIdsServiceJob;
import org.smartregister.job.SyncServiceJob;
import org.smartregister.job.ValidateSyncDataServiceJob;
import org.smartregister.reveal.BaseUnitTest;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 * Created by Richard Kareko on 7/29/20.
 */

public class RevealJobCreatorTest extends BaseUnitTest {
    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private RevealJobCreator revealJobCreator;

    @Before
    public void setUp() {
        revealJobCreator = new RevealJobCreator();
    }

    @Test
    public void testCreateP2pServiceJob() {
        Job p2pServiceJob = revealJobCreator.create(P2pServiceJob.TAG);
        assertNotNull(p2pServiceJob);
        assertTrue(p2pServiceJob instanceof  P2pServiceJob);
    }

    @Test
    public void testCreateSyncServiceJob() {
        Job syncServiceJob = revealJobCreator.create(SyncServiceJob.TAG);
        assertNotNull(syncServiceJob);
        assertTrue(syncServiceJob instanceof  SyncServiceJob);
    }

    @Test
    public void testCreateLocationTaskServiceJob() {
        Job locationTaskServiceJob = revealJobCreator.create(LocationTaskServiceJob.TAG);
        assertNotNull(locationTaskServiceJob);
        assertTrue(locationTaskServiceJob instanceof  LocationTaskServiceJob);
    }

    @Test
    public void testCreateRevealSyncSettingsServiceJob() {
        Job revealSyncSettingsServiceJob = revealJobCreator.create(RevealSyncSettingsServiceJob.TAG);
        assertNotNull(revealSyncSettingsServiceJob);
        assertTrue(revealSyncSettingsServiceJob instanceof  RevealSyncSettingsServiceJob);
    }

    @Test
    public void testExtendedSyncServiceJob() {
        Job extendedSyncServiceJob = revealJobCreator.create(ExtendedSyncServiceJob.TAG);
        assertNotNull(extendedSyncServiceJob);
        assertTrue(extendedSyncServiceJob instanceof  ExtendedSyncServiceJob);
    }

    @Test
    public void testCreatePullUniqueIdsServiceJob() {
        Job pullUniqueIdsServiceJob = revealJobCreator.create(PullUniqueIdsServiceJob.TAG);
        assertNotNull(pullUniqueIdsServiceJob);
        assertTrue(pullUniqueIdsServiceJob instanceof PullUniqueIdsServiceJob);
    }

    @Test
    public void testValidateSyncDataServiceJob() {
        Job validateSyncDataServiceJob = revealJobCreator.create(ValidateSyncDataServiceJob.TAG);
        assertNotNull(validateSyncDataServiceJob);
        assertTrue(validateSyncDataServiceJob instanceof ValidateSyncDataServiceJob);
    }

    @Test
    public void testCreateDocumentConfigurationServiceJob() {
        Job documentConfigurationServiceJob = revealJobCreator.create(DocumentConfigurationServiceJob.TAG);
        assertNotNull(documentConfigurationServiceJob);
        assertTrue(documentConfigurationServiceJob instanceof  DocumentConfigurationServiceJob);
    }

    @Test
    public void testCreateJobUsingNonExistingTagReturnsNull() {
        Job nonExistingTagServiceJob = revealJobCreator.create("test-tag");
        assertNull(nonExistingTagServiceJob);
    }

}
