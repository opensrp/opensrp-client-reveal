package org.smartregister.reveal.job;

import com.evernote.android.job.Job;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.smartregister.job.P2pServiceJob;
import org.smartregister.reveal.BaseUnitTest;

import static org.junit.Assert.assertNotNull;
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
}
