package org.smartregister.reveal.interactor;

import org.junit.Test;
import org.smartregister.reveal.util.AppExecutors;

import static org.mockito.ArgumentMatchers.anyObject;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.powermock.api.mockito.PowerMockito.doNothing;
import static org.powermock.api.mockito.PowerMockito.verifyPrivate;

/**
 * @author Vincent Karuri
 */
public class ListTaskInteractorTest {

    private ListTaskInteractor listTaskInteractor = new ListTaskInteractor(new AppExecutors());

    @Test
    public void testSaveJsonFormShouldSaveMosquitoCollectionFormIfEventTypeIsMosquitoCollection() throws Exception {
        String mosquitoCollectionForm = "{\n" +
                "  \"baseEntityId\": \"227ce82f-d688-467a-97d7-bdad30290cea\",\n" +
                "  \"duration\": 0,\n" +
                "  \"entityType\": \"Structure\",\n" +
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

        ListTaskInteractor listTaskInteractorSpy = spy(listTaskInteractor);
        doNothing().when(listTaskInteractorSpy, "saveMosquitoCollectionForm", anyObject());
        listTaskInteractorSpy.saveJsonForm(mosquitoCollectionForm);
        verifyPrivate(listTaskInteractorSpy, times(1)).invoke("saveMosquitoCollectionForm", anyObject());
    }
}
