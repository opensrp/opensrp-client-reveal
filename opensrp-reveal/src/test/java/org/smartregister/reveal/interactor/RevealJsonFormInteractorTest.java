package org.smartregister.reveal.interactor;

import com.vijay.jsonwizard.interactors.JsonFormInteractor;

import org.junit.Test;
import org.smartregister.reveal.BaseUnitTest;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;

public class RevealJsonFormInteractorTest extends BaseUnitTest {

    @Test
    public void testGetInstance() {
        assertThat(RevealJsonFormInteractor.getInstance(), is(instanceOf(JsonFormInteractor.class)));
    }

}