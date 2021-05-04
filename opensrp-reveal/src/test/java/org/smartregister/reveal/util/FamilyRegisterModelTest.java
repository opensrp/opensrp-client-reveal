package org.smartregister.reveal.util;

import com.vijay.jsonwizard.utils.FormUtils;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.clientandeventmodel.Client;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.domain.Task;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.model.FamilyRegisterModel;
import org.smartregister.util.JsonFormUtils;

import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_UUID;
import static org.smartregister.reveal.util.Constants.Properties.PLAN_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.Properties.TASK_STATUS;
import static org.smartregister.reveal.util.FamilyConstants.DatabaseKeys.FAMILY_NAME;
import static org.smartregister.reveal.util.FamilyConstants.RELATIONSHIP.RESIDENCE;

/**
 * Created by Richard Kareko on 4/16/20.
 */

public class FamilyRegisterModelTest extends BaseUnitTest {
    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    public FormUtils formUtils;

    @Captor
    private ArgumentCaptor<String> formNameCaptor;

    private FamilyRegisterModel familyRegisterModel;

    @Before
    public void setUp() {
        familyRegisterModel = new FamilyRegisterModel("struct1", "task1", COMPLETE,
                Task.TaskStatus.READY.name(), "test house");
    }

    @Test
    public void testInitialization() {
        assertEquals("struct1", Whitebox.getInternalState(familyRegisterModel, "structureId"));
        assertEquals("task1", Whitebox.getInternalState(familyRegisterModel, "taskId"));
        assertEquals(COMPLETE, Whitebox.getInternalState(familyRegisterModel, "taskBusinessStatus"));
        assertEquals(Task.TaskStatus.READY.name(), Whitebox.getInternalState(familyRegisterModel, "taskStatus"));
        assertEquals("test house", Whitebox.getInternalState(familyRegisterModel, "structureName"));
    }

    @Test
    public void testProcessRegistration() {
        String expectedPlanId = "a8b3010c-1ba5-556d-8b16-71266397b8b9";
        PreferencesUtil.getInstance().setCurrentPlanId(expectedPlanId);
        List<FamilyEventClient> actualEventClientList = familyRegisterModel.processRegistration(TestingUtils.familyRegJSON);

        assertEquals(2, actualEventClientList.size());
        Client actualClient = actualEventClientList.get(0).getClient();
        Event actualEvent = actualEventClientList.get(0).getEvent();
        assertEquals("struct1", actualClient.getAttribute(RESIDENCE));
        assertEquals("task1", actualEvent.getDetails().get(TASK_IDENTIFIER));
        assertEquals(COMPLETE, actualEvent.getDetails().get(TASK_BUSINESS_STATUS));
        assertEquals("struct1", actualEvent.getDetails().get(LOCATION_UUID));
        assertEquals(Task.TaskStatus.READY.name(), actualEvent.getDetails().get(TASK_STATUS));
        assertEquals(expectedPlanId, actualEvent.getDetails().get(PLAN_IDENTIFIER));

    }

    @Test
    public void testGetFormAsJson() throws Exception {
        JSONObject formObject = new JSONObject(TestingUtils.familyRegJSON);
        when(formUtils.getFormJsonFromRepositoryOrAssets(any(), any())).thenReturn(formObject);
        Whitebox.setInternalState(familyRegisterModel, "formUtils", formUtils);
        JSONObject resultJson = familyRegisterModel.getFormAsJson("json.form/family_register.json", "131", "131");

        verify(formUtils).getFormJsonFromRepositoryOrAssets(any(), formNameCaptor.capture());
        assertEquals("family_register", formNameCaptor.getValue());
        assertEquals("test house", JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(resultJson), FAMILY_NAME).getString("value"));
    }

    @Test
    public void testGetStructure() {
        assertEquals("struct1", familyRegisterModel.getStructureId());
    }
}
