package org.smartregister.reveal.util;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.powermock.reflect.Whitebox;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import static org.junit.Assert.assertEquals;

public class RepeatingGroupGeneratorTest {

    private RepeatingGroupGenerator repeatingGroupGenerator;

    @Before
    public void setup() {
        repeatingGroupGenerator = new RepeatingGroupGenerator(new JSONObject(),
             //   JsonFormConstants.STEP1,
                "",
                new HashMap<>(),
                Constants.JsonForm.REPEATING_GROUP_UNIQUE_ID,
                new LinkedList<>());
    }

    @Test
    public void testProcessColumnValueDob() {
        String dob = repeatingGroupGenerator.processColumnValue("dob", "2020-12-12T07:22:05Z");
        assertEquals("12-12-2020", dob);
    }

    @Test
    public void testGetBaseEntityId() {
        Whitebox.setInternalState(repeatingGroupGenerator, "baseEntityId", "dummy");
        assertEquals("dummy", repeatingGroupGenerator.getBaseEntityId());
    }

    @Test
    public void testSetHiddenFields() {
        Set<String> expectedList = new HashSet<>();
        repeatingGroupGenerator.setHiddenFields(expectedList);
        assertEquals(expectedList, Whitebox.getInternalState(repeatingGroupGenerator, "hiddenFields"));
    }

    @Test
    public void testSetFieldsWithoutSpecialViewValidation() {
        Set<String> expectedList = new HashSet<>();
        repeatingGroupGenerator.setFieldsWithoutSpecialViewValidation(expectedList);
        assertEquals(expectedList, Whitebox.getInternalState(repeatingGroupGenerator, "fieldsWithoutSpecialViewValidation"));
    }

    @Test
    public void testSetFieldsWithoutRefreshLogic() {
        Set<String> expectedList = new HashSet<>();
        repeatingGroupGenerator.setFieldsWithoutRefreshLogic(expectedList);
        assertEquals(expectedList, Whitebox.getInternalState(repeatingGroupGenerator, "fieldsWithoutRefreshLogic"));
    }

    @Test
    public void testSetReadOnlyFields() {
        Set<String> expectedList = new HashSet<>();
        repeatingGroupGenerator.setReadOnlyFields(expectedList);
        assertEquals(expectedList, Whitebox.getInternalState(repeatingGroupGenerator, "readOnlyFields"));
    }
}
