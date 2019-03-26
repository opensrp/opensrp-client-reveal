package org.smartregister.reveal.interactor;

import net.sqlcipher.Cursor;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;
import org.smartregister.reveal.model.MosquitoCollectionCardDetails;
import org.smartregister.reveal.model.SprayCardDetails;
import org.smartregister.reveal.util.AppExecutors;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.doNothing;
import static org.powermock.api.mockito.PowerMockito.verifyPrivate;

/**
 * @author Vincent Karuri
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest(ListTaskInteractor.class)
public class ListTaskInteractorTest {

    private ListTaskInteractor listTaskInteractor;

    private final  String mosquitoCollectionForm = "{\n" +
            "  \"baseEntityId\": \"227ce82f-d688-467a-97d7-bdad30290cea\",\n" +
            "  \"duration\": 0,\n" +
            "  \"entityType\": \"Structure\",\n" +
            "  \"encounter_type\": \"mosquito_collection\",\n" +
            "  \"eventDate\": \"2019-03-18T00:00:00.000+0000\",\n" +
            "  \"eventType\": \"mosquito_collection\",\n" +
            "  \"formSubmissionId\": \"cfd96619-5850-4277-b2b9-f30b0f2c0944\",\n" +
            "  \"locationId\": \"18e9f800-55c7-4261-907a-d804d6081f93\",\n" +
            "  \"obs\": [\n" +
            "    {\n" +
            "      \"fieldCode\": \"structure_type\",\n" +
            "      \"fieldDataType\": \"text\",\n" +
            "      \"fieldType\": \"formsubmissionField\",\n" +
            "      \"formSubmissionField\": \"structure_type\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"Facility\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"trap_location\",\n" +
            "      \"fieldDataType\": \"text\",\n" +
            "      \"fieldType\": \"formsubmissionField\",\n" +
            "      \"formSubmissionField\": \"trap_location\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"Indoor\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"trap_start\",\n" +
            "      \"fieldDataType\": \"text\",\n" +
            "      \"fieldType\": \"formsubmissionField\",\n" +
            "      \"formSubmissionField\": \"trap_start\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"18-03-2019\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"trap_end\",\n" +
            "      \"fieldDataType\": \"text\",\n" +
            "      \"fieldType\": \"formsubmissionField\",\n" +
            "      \"formSubmissionField\": \"trap_end\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"18-03-2019\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"trap_temp\",\n" +
            "      \"fieldDataType\": \"text\",\n" +
            "      \"fieldType\": \"formsubmissionField\",\n" +
            "      \"formSubmissionField\": \"trap_temp\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"36\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"trap_RH\",\n" +
            "      \"fieldDataType\": \"text\",\n" +
            "      \"fieldType\": \"formsubmissionField\",\n" +
            "      \"formSubmissionField\": \"trap_RH\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"69.5\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"An. funestus\",\n" +
            "      \"fieldDataType\": \"text\",\n" +
            "      \"fieldType\": \"formsubmissionField\",\n" +
            "      \"formSubmissionField\": \"An. funestus\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"45\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"An. dirus\",\n" +
            "      \"fieldDataType\": \"text\",\n" +
            "      \"fieldType\": \"formsubmissionField\",\n" +
            "      \"formSubmissionField\": \"An. dirus\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"63\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"163137AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\n" +
            "      \"fieldDataType\": \"start\",\n" +
            "      \"fieldType\": \"concept\",\n" +
            "      \"formSubmissionField\": \"start\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"2019-03-18 09:23:30\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"163138AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\n" +
            "      \"fieldDataType\": \"end\",\n" +
            "      \"fieldType\": \"concept\",\n" +
            "      \"formSubmissionField\": \"end\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"2019-03-18 09:24:04\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"163149AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\n" +
            "      \"fieldDataType\": \"deviceid\",\n" +
            "      \"fieldType\": \"concept\",\n" +
            "      \"formSubmissionField\": \"deviceid\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"000000000000000\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"163150AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\n" +
            "      \"fieldDataType\": \"subscriberid\",\n" +
            "      \"fieldType\": \"concept\",\n" +
            "      \"formSubmissionField\": \"subscriberid\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"310260000000000\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"163151AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\n" +
            "      \"fieldDataType\": \"simserial\",\n" +
            "      \"fieldType\": \"concept\",\n" +
            "      \"formSubmissionField\": \"simserial\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"89014103211118510720\"\n" +
            "      ]\n" +
            "    },\n" +
            "    {\n" +
            "      \"fieldCode\": \"163152AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\n" +
            "      \"fieldDataType\": \"phonenumber\",\n" +
            "      \"fieldType\": \"concept\",\n" +
            "      \"formSubmissionField\": \"phonenumber\",\n" +
            "      \"humanReadableValues\": [],\n" +
            "      \"parentCode\": \"\",\n" +
            "      \"values\": [\n" +
            "        \"15555215554\"\n" +
            "      ]\n" +
            "    }\n" +
            "  ],\n" +
            "  \"providerId\": \"demomti\",\n" +
            "  \"team\": \"Miti\",\n" +
            "  \"teamId\": \"7e104eee-ec8a-4733-bcf7-c02c51cf43f4\",\n" +
            "  \"version\": 1552901044526,\n" +
            "  \"dateCreated\": \"2019-03-18T09:24:04.527+0000\",\n" +
            "  \"type\": \"Event\",\n" +
            "  \"details\": {\n" +
            "    \"taskIdentifier\": \"804833dc-5120-4290-9e3e-6bffd7075c64\",\n" +
            "    \"taskBusinessStatus\": \"Not Visited\",\n" +
            "    \"taskStatus\": \"READY\",\n" +
            "    \"locationUUID\": \"1bff00f6-0408-49e5-b53e-9dedeeb3b04e\",\n" +
            "    \"locationVersion\": \"0\"\n" +
            "  }\n" +
            "}";

    @Captor
    private ArgumentCaptor<JSONObject> captor;


    @Before
    public void setUp() {
        listTaskInteractor = new ListTaskInteractor(new AppExecutors());
    }

    @Test
    public void testSaveJsonFormShouldSaveMosquitoCollectionForm() throws Exception {
        ListTaskInteractor listTaskInteractorSpy = PowerMockito.spy(listTaskInteractor);

        doNothing().when(listTaskInteractorSpy, "saveMosquitoCollectionForm", any(JSONObject.class));
        listTaskInteractorSpy.saveJsonForm(mosquitoCollectionForm);

        verifyPrivate(listTaskInteractorSpy, times(1)).invoke("saveMosquitoCollectionForm", captor.capture());
        assertEquals(new JSONObject(mosquitoCollectionForm).toString(), captor.getValue().toString());
    }

    @Test
    public void testCreateMosquitoCollectionCardDetailsShouldPopulateCorrectCardDetails() throws Exception {
        final String STATUS = "active";
        final String START_DATE = "11/02/1977";
        final String END_DATE = "23/04/1990";

        Cursor cursor = mock(Cursor.class);
        when(cursor.getColumnIndex("status")).thenReturn(0);
        when(cursor.getColumnIndex("start_date")).thenReturn(1);
        when(cursor.getColumnIndex("end_date")).thenReturn(2);

        when(cursor.getString(0)).thenReturn(STATUS);
        when(cursor.getString(1)).thenReturn(START_DATE);
        when(cursor.getString(2)).thenReturn(END_DATE);

        MosquitoCollectionCardDetails mosquitoCollectionCardDetails = Whitebox.invokeMethod(listTaskInteractor, "createMosquitoCollectionCardDetails", cursor);

        assertEquals(mosquitoCollectionCardDetails.getStatus(), STATUS);
        assertEquals(mosquitoCollectionCardDetails.getTrapSetDate(), START_DATE);
        assertEquals(mosquitoCollectionCardDetails.getTrapFollowUpDate(), END_DATE);
    }

    @Test
    public void testCreateSprayCardDetailsShouldPopulateCorrectCardDetails() throws Exception {
        final String PROPERTY_TYPE = "Residential";
        final String SPRAY_DATE = "11/02/1977";
        final String SPRAY_OPERATOR = "John Doe";
        final String FAMILY_HEAD = "Doe John";

        Cursor cursor = mock(Cursor.class);
        when(cursor.getColumnIndex("property_type")).thenReturn(0);
        when(cursor.getColumnIndex("spray_date")).thenReturn(1);
        when(cursor.getColumnIndex("spray_operator")).thenReturn(2);
        when(cursor.getColumnIndex("family_head_name")).thenReturn(3);

        when(cursor.getString(0)).thenReturn(PROPERTY_TYPE);
        when(cursor.getString(1)).thenReturn(SPRAY_DATE);
        when(cursor.getString(2)).thenReturn(SPRAY_OPERATOR);
        when(cursor.getString(3)).thenReturn(FAMILY_HEAD);

        SprayCardDetails sprayCardDetails = Whitebox.invokeMethod(listTaskInteractor, "createSprayCardDetails", cursor);

        assertEquals(sprayCardDetails.getPropertyType(), PROPERTY_TYPE);
        assertEquals(sprayCardDetails.getSprayDate(), SPRAY_DATE);
        assertEquals(sprayCardDetails.getSprayOperator(), SPRAY_OPERATOR);
        assertEquals(sprayCardDetails.getFamilyHead(), FAMILY_HEAD);
    }
}
