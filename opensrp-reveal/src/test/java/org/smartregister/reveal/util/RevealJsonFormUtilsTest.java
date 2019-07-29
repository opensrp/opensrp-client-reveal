package org.smartregister.reveal.util;

import org.junit.Before;
import org.junit.Test;
import org.powermock.reflect.Whitebox;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.util.Constants.JsonForm;

import static org.junit.Assert.assertEquals;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.LARVAL_DIPPING_EVENT;
import static org.smartregister.reveal.util.Constants.MOSQUITO_COLLECTION_EVENT;

/**
 * Created by Vincent Karuri on 25/04/2019
 */
public class RevealJsonFormUtilsTest {

    private RevealJsonFormUtils revealJsonFormUtils;

    @Before
    public void setUp() {
        revealJsonFormUtils = new RevealJsonFormUtils();
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.ZAMBIA);
    }

    @Test
    public void testGetFormNameShouldReturnThailandLarvalDippingFormForLarvalDippingEvent() {
        assertEquals(revealJsonFormUtils.getFormName(LARVAL_DIPPING_EVENT, null), JsonForm.LARVAL_DIPPING_FORM);
    }

    @Test
    public void testGetFormNameShouldReturnThailandLarvalDippingFormForLarvalDippingIntervention() {
        assertEquals(revealJsonFormUtils.getFormName(null, LARVAL_DIPPING), JsonForm.LARVAL_DIPPING_FORM);
    }

    @Test
    public void testGetFormNameShouldReturnThailandMosquitoCollectionFormForLarvalDippingEvent() {
        assertEquals(revealJsonFormUtils.getFormName(MOSQUITO_COLLECTION_EVENT, null), JsonForm.MOSQUITO_COLLECTION_FORM);
    }

    @Test
    public void testGetFormNameShouldReturnThailandMosquitoCollectionFormForLarvalDippingIntervention() {
        assertEquals(revealJsonFormUtils.getFormName(null, MOSQUITO_COLLECTION), JsonForm.MOSQUITO_COLLECTION_FORM);
    }

    @Test
    public void testGetFormNameShouldReturnPAOTForm() {
        assertEquals(JsonForm.PAOT_FORM, revealJsonFormUtils.getFormName(null, Constants.EventType.PAOT_EVENT));
    }

    @Test
    public void testGetFormNameShouldReturnThailandPAOTForm() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.THAILAND);
        assertEquals(JsonForm.THAILAND_PAOT_FORM, revealJsonFormUtils.getFormName(null, Constants.EventType.PAOT_EVENT));
    }
}
