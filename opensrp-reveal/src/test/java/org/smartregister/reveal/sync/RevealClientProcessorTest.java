package org.smartregister.reveal.sync;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.domain.db.Event;
import org.smartregister.domain.db.Obs;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.util.Constants.JsonForm;
import org.smartregister.util.JsonFormUtils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * Created by samuelgithengi on 3/13/19.
 */
public class RevealClientProcessorTest extends BaseUnitTest {


    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private RevealClientProcessor clientProcessor;

    private Event event;

    private Event sprayedEvent;

    @Before
    public void setUp() {
        clientProcessor = new RevealClientProcessor(RuntimeEnvironment.application);
        String eventJSON = "{\"baseEntityId\":\"b9f60dfd-799e-41f7-9d3c-1370d894bc6d\",\"duration\":0,\"entityType\":\"Structure\",\"eventDate\":\"2019-03-07T00:00:00.000+0100\",\"eventType\":\"Spray\",\"formSubmissionId\":\"f4423e04-047a-40d2-b5f2-baf2a9c831b2\",\"locationId\":\"79496c6b-cb29-405a-bcdd-0dcf8afddf55\",\"obs\":[{\"fieldCode\":\"structureType\",\"fieldDataType\":\"text\",\"fieldType\":\"formsubmissionField\",\"formSubmissionField\":\"structureType\",\"humanReadableValues\":[],\"parentCode\":\"\",\"values\":[\"Non-Residential Structure\"]},{\"fieldCode\":\"visit_number\",\"fieldDataType\":\"text\",\"fieldType\":\"formsubmissionField\",\"formSubmissionField\":\"visit_number\",\"humanReadableValues\":[],\"parentCode\":\"\",\"values\":[\"Mop-up\"]},{\"fieldCode\":\"nonresidentialtype\",\"fieldDataType\":\"text\",\"fieldType\":\"formsubmissionField\",\"formSubmissionField\":\"nonresidentialtype\",\"humanReadableValues\":[],\"parentCode\":\"\",\"values\":[\"Offices\"]},{\"fieldCode\":\"business_status\",\"fieldDataType\":\"text\",\"fieldType\":\"formsubmissionField\",\"formSubmissionField\":\"business_status\",\"humanReadableValues\":[],\"parentCode\":\"\",\"values\":[\"Not Sprayable\"]},{\"fieldCode\":\"163137AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"fieldDataType\":\"start\",\"fieldType\":\"concept\",\"formSubmissionField\":\"start\",\"humanReadableValues\":[],\"parentCode\":\"\",\"values\":[\"2019-03-07 16:47:04\"]},{\"fieldCode\":\"163138AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"fieldDataType\":\"end\",\"fieldType\":\"concept\",\"formSubmissionField\":\"end\",\"humanReadableValues\":[],\"parentCode\":\"\",\"values\":[\"2019-03-07 16:47:32\"]},{\"fieldCode\":\"163149AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"fieldDataType\":\"deviceid\",\"fieldType\":\"concept\",\"formSubmissionField\":\"deviceid\",\"humanReadableValues\":[],\"parentCode\":\"\",\"values\":[\"358240051111110\"]},{\"fieldCode\":\"163150AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"fieldDataType\":\"subscriberid\",\"fieldType\":\"concept\",\"formSubmissionField\":\"subscriberid\",\"humanReadableValues\":[],\"parentCode\":\"\",\"values\":[\"310260000000000\"]},{\"fieldCode\":\"163151AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"fieldDataType\":\"simserial\",\"fieldType\":\"concept\",\"formSubmissionField\":\"simserial\",\"humanReadableValues\":[],\"parentCode\":\"\",\"values\":[\"89014103211118510720\"]},{\"fieldCode\":\"163152AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"fieldDataType\":\"phonenumber\",\"fieldType\":\"concept\",\"formSubmissionField\":\"phonenumber\",\"humanReadableValues\":[],\"parentCode\":\"\",\"values\":[\"+15555215554\"]}],\"providerId\":\"swana\",\"team\":\"Botswana\",\"teamId\":\"e5470c0b-2349-45ec-bdd1-e0ed5a120a87\",\"version\":1551973652706,\"dateCreated\":\"2019-03-07T16:47:32.706+0100\",\"type\":\"Event\",\"details\":{\"taskIdentifier\":\"c5352384-6578-4477-bd66-1fa95f4006c7\",\"taskBusinessStatus\":\"Not Visited\",\"taskStatus\":\"READY\",\"locationUUID\":\"0d9e76c7-5b83-4fe3-8a50-e3f0dd8d883e\",\"locationVersion\":\"0\"}}";
        event = JsonFormUtils.gson.fromJson(eventJSON, Event.class);


        String sprayedEventJSON = "{\"identifiers\":{},\"baseEntityId\":\"156727\",\"locationId\":\"18e9f800-55c7-4261-907a-d804d6081f93\",\"eventDate\":\"2018-12-19T07:00:00.000+02:00\",\"eventType\":\"Spray\",\"formSubmissionId\":\"f46c3f90-54fc-40bc-bcc9-a7938eea4303\",\"providerId\":\"demoMTI\",\"duration\":0,\"obs\":[{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"structureType\",\"parentCode\":\"\",\"values\":[\"Residential Structure\"],\"set\":[],\"formSubmissionField\":\"structureType\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"sprayStatus\",\"parentCode\":\"\",\"values\":[\"Sprayed\"],\"set\":[],\"formSubmissionField\":\"sprayStatus\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"notSprayedReason\",\"parentCode\":\"\",\"values\":[\"No one home/Missed\"],\"set\":[],\"formSubmissionField\":\"notSprayedReason\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"familyHeadName\",\"parentCode\":\"\",\"values\":[\"Berg\"],\"set\":[],\"formSubmissionField\":\"familyHeadName\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"familyHeadSex\",\"parentCode\":\"\",\"values\":[\"Male\"],\"set\":[],\"formSubmissionField\":\"familyHeadSex\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"roomsFound\",\"parentCode\":\"\",\"values\":[\"2\"],\"set\":[],\"formSubmissionField\":\"roomsFound\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"sprayedRooms\",\"parentCode\":\"\",\"values\":[\"2\"],\"set\":[],\"formSubmissionField\":\"sprayedRooms\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"netsFound\",\"parentCode\":\"\",\"values\":[\"2\"],\"set\":[],\"formSubmissionField\":\"netsFound\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"mopUp\",\"parentCode\":\"\",\"values\":[\"No\"],\"set\":[],\"formSubmissionField\":\"mopUp\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"start\",\"fieldCode\":\"163137AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"2018-12-19 20:19:00\"],\"set\":[],\"formSubmissionField\":\"start\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"end\",\"fieldCode\":\"163138AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"2018-12-19 20:19:25\"],\"set\":[],\"formSubmissionField\":\"end\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"deviceid\",\"fieldCode\":\"163149AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"000000000000000\"],\"set\":[],\"formSubmissionField\":\"deviceid\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"subscriberid\",\"fieldCode\":\"163150AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"310270000000000\"],\"set\":[],\"formSubmissionField\":\"subscriberid\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"simserial\",\"fieldCode\":\"163151AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"89014103211118510720\"],\"set\":[],\"formSubmissionField\":\"simserial\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"phonenumber\",\"fieldCode\":\"163152AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"15555218135\"],\"set\":[],\"formSubmissionField\":\"phonenumber\",\"humanReadableValues\":[]}],\"entityType\":\"Structure\",\"details\":{\"taskStatus\":\"READY\",\"taskIdentifier\":\"634fa9fa-736d-4298-96aa-3de68ac02cae\",\"locationVersion\":\"0\",\"taskBusinessStatus\":\"Not Visited\"},\"version\":1545268765121,\"teamId\":\"7e104eee-ec8a-4733-bcf7-c02c51cf43f4\",\"team\":\"Miti\",\"dateCreated\":\"2018-12-20T03:31:37.092+02:00\",\"serverVersion\":1545269497091,\"type\":\"Event\",\"id\":\"4a5e9539-7433-4742-a3ea-6eb6117b4b51\",\"revision\":\"v1\"}";
        sprayedEvent = JsonFormUtils.gson.fromJson(sprayedEventJSON, Event.class);
    }

    @Test
    public void testCalculateBusinessStatusUsingCalculatedFormStatus() {
        assertEquals("Not Sprayable", clientProcessor.calculateBusinessStatus(event));
        assertEquals("Sprayed", clientProcessor.calculateBusinessStatus(sprayedEvent));
    }

    @Test
    public void testCalculateBusinessStatusWithoutCalculatedFormStatus() {
        Obs businessStatusObs = event.findObs(null, false, JsonForm.TASK_BUSINESS_STATUS);
        event.getObs().remove(businessStatusObs);
        assertEquals("Not Sprayable", clientProcessor.calculateBusinessStatus(event));


        businessStatusObs = sprayedEvent.findObs(null, false, JsonForm.TASK_BUSINESS_STATUS);
        sprayedEvent.getObs().remove(businessStatusObs);
        assertEquals("Sprayed", clientProcessor.calculateBusinessStatus(sprayedEvent));
    }

    @Test
    public void testCalculateBusinessStatusWithoutStructureType() {
        Obs businessStatusObs = event.findObs(null, false, JsonForm.TASK_BUSINESS_STATUS);
        Obs structureTypeObs = event.findObs(null, false, JsonForm.STRUCTURE_TYPE);
        event.getObs().remove(businessStatusObs);
        event.getObs().remove(structureTypeObs);
        assertNull(clientProcessor.calculateBusinessStatus(event));
    }

    @Test
    public void testCalculateBusinessStatusUsingNullSprayStatus() {
        Obs businessStatusObs = sprayedEvent.findObs(null, false, JsonForm.TASK_BUSINESS_STATUS);
        sprayedEvent.getObs().remove(businessStatusObs);

        Obs sprayStatus = sprayedEvent.findObs(null, false, JsonForm.SPRAY_STATUS);
        sprayedEvent.getObs().remove(sprayStatus);

        assertNull(clientProcessor.calculateBusinessStatus(sprayedEvent));
    }


    @Test
    public void testCalculateBusinessStatusUsingSprayStatus() {
        Obs businessStatusObs = sprayedEvent.findObs(null, false, JsonForm.TASK_BUSINESS_STATUS);
        sprayedEvent.getObs().remove(businessStatusObs);

        assertEquals("Sprayed", clientProcessor.calculateBusinessStatus(sprayedEvent));
    }

}
