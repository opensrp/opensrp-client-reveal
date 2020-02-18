package org.smartregister.reveal.util;

import android.location.Location;
import android.support.v4.util.Pair;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.mapbox.geojson.Feature;
import com.mapbox.mapboxsdk.offline.OfflineRegion;

import net.sqlcipher.MatrixCursor;

import org.joda.time.DateTime;
import org.json.JSONObject;
import org.mockito.Mockito;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Task;
import org.smartregister.domain.Task.TaskStatus;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Constants.InterventionType;
import org.smartregister.util.DateTimeTypeConverter;

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import io.ona.kujaku.data.realm.objects.MapBoxOfflineQueueTask;

import static io.ona.kujaku.downloaders.MapBoxOfflineResourcesDownloader.METADATA_JSON_FIELD_REGION_NAME;
import static org.smartregister.family.util.DBConstants.KEY;
import static org.smartregister.reveal.model.OfflineMapModel.OfflineMapStatus.DOWNLOADED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CODE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FOR;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.GROUPID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;

/**
 * Created by samuelgithengi on 3/27/19.
 */
public class TestingUtils {

    public static Gson gson = new GsonBuilder().setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
            .registerTypeAdapter(DateTime.class, new DateTimeTypeConverter()).create();

    public static String operationalAreaGeoJSON = "{\"geometry\":{\"coordinates\":[[[[32.6438933251249,-14.150522101063697],[32.643924247229386,-14.150527608935132],[32.64398139683106,-14.15052154398239],[32.644081650000025,-14.150518009902552],[32.64418190316899,-14.15052154398239],[32.64428163367375,-14.150532127797131],[32.64438032157448,-14.1505497061687],[32.644477452366736,-14.150574187453056],[32.6445725196641,-14.150605444018307],[32.64466502783676,-14.150643312909946],[32.644754494596434,-14.150687596700205],[32.64484045351036,-14.150738064517608],[32.644922456433214,-14.15079445325011],[32.645000075842454,-14.150856468917194],[32.645072907068474,-14.15092378820219],[32.64514057040332,-14.150996060138025],[32.645202713081,-14.15107290793679],[32.64525901111619,-14.151153930953907],[32.64530917099352,-14.15123870677689],[32.645352931198026,-14.151326793427753],[32.64539006357858,-14.151417731667092],[32.64542037453713,-14.151511047387995],[32.645443706039025,-14.151606254088048],[32.64544940477543,-14.151640172262542],[32.64546926711102,-14.151701320661399],[32.645492598632465,-14.15179652735986],[32.645508829043244,-14.151893128675962],[32.64551787371692,-14.151990620976333],[32.64551968548948,-14.152088495982294],[32.645514254906054,-14.152186243418836],[32.64550161026977,-14.152283353675838],[32.64548181749506,-14.152379320464032],[32.64545497976376,-14.15247364345549],[32.64542123698737,-14.152565830891316],[32.64538076507795,-14.152655402146364],[32.6453337750309,-14.15274189023398],[32.64528051182548,-14.152824844242101],[32.645221253147376,-14.152903831682407],[32.645156307941015,-14.152978440747173],[32.64509273402205,-14.153041606383848],[32.64506924207182,-14.153124170165423],[32.64503549919947,-14.15321635759636],[32.644995027174836,-14.153305928846518],[32.6449480369942,-14.153392416929588],[32.6448947736373,-14.153475370933105],[32.644835514790536,-14.153554358369146],[32.644770569399434,-14.153628967429764],[32.64470027605761,-14.153698809133571],[32.64462500124176,-14.1537635193535],[32.64454513740122,-14.153822760716249],[32.64446110091148,-14.153876224360427],[32.644373329903594,-14.153923631547226],[32.644282281979876,-14.153964735113899],[32.64418843182823,-14.153999320762008],[32.64409226874664,-14.15402720817502],[32.643994294092856,-14.154048251958553],[32.643895018669575,-14.154062342398216],[32.64379496006193,-14.15406940603162],[32.643783032899755,-14.15406940603162],[32.64371400152287,-14.154074279298753],[32.643613681397134,-14.154074279298753],[32.64351362278739,-14.154067215665403],[32.643414347362004,-14.154053125225742],[32.64336158499048,-14.154041792499807],[32.64332821566398,-14.154048959831865],[32.643228940240355,-14.154063050271416],[32.64312888163237,-14.154070113904762],[32.64302856150845,-14.154070113904762],[32.64292850290047,-14.154063050271416],[32.642829227476845,-14.154048959831865],[32.642731252822664,-14.154027916048332],[32.6426350897409,-14.154000028635263],[32.642541239588915,-14.153965442987154],[32.64245019166486,-14.153924339420538],[32.6423624206568,-14.153876932233569],[32.642278384166666,-14.15382346858939],[32.642198520325785,-14.153764227226812],[32.64212324550976,-14.153699517006768],[32.64205295216777,-14.153629675303076],[32.64198800677644,-14.153555066242458],[32.64192874792956,-14.153476078806303],[32.64187548457249,-14.153393124802902],[32.641828494391625,-14.15330663671983],[32.64178802236699,-14.153217065469672],[32.64175427949447,-14.153124878038735],[32.641727441686776,-14.153030555052336],[32.64171533051836,-14.152971833292439],[32.6416843506609,-14.152887194485116],[32.64165751288129,-14.152792871496896],[32.64163772007071,-14.152696904711998],[32.64162507541169,-14.152599794458407],[32.64161964481848,-14.152502047025106],[32.64162145659423,-14.15240417202261],[32.64163050128439,-14.152306679725537],[32.6416467317245,-14.152210078412732],[32.64167006328801,-14.152114871717568],[32.64168810456106,-14.152059329764139],[32.6416688677155,-14.15199172047431],[32.64164907497439,-14.151895753682933],[32.64163643035959,-14.151798643422918],[32.6416309997852,-14.15170089598325],[32.64163281155481,-14.151603020974104],[32.641641856213134,-14.151505528670551],[32.6416580865964,-14.151408927351438],[32.64168141807806,-14.151313720649794],[32.64171172901063,-14.151220404927411],[32.64174886135901,-14.151129466686594],[32.64179262152572,-14.151041380034197],[32.64184278135968,-14.150956604209792],[32.64189907934644,-14.150875581191483],[32.641961221970455,-14.150798733391408],[32.642028885247036,-14.150726461454381],[32.64210171641002,-14.150659142168248],[32.64217933575241,-14.150597126500141],[32.64226133860438,-14.150540737766729],[32.64234729744424,-14.150490269948532],[32.642436764126664,-14.150445986157534],[32.6425292722194,-14.150408117265213],[32.64262433943485,-14.150376860699449],[32.64272147014332,-14.150352379414695],[32.6428201579589,-14.150334801042785],[32.642919888377484,-14.15032421722793],[32.643020141459935,-14.150320683148035],[32.64312039454261,-14.15032421722793],[32.643220124961196,-14.150334801042785],[32.643318812776776,-14.150352379414695],[32.643415943485245,-14.150376860699449],[32.64351101070053,-14.150408117265213],[32.643603518793434,-14.150445986157534],[32.64369298547586,-14.150490269948532],[32.643725059780984,-14.150509101289227],[32.643793594624746,-14.150511517249011],[32.6438933251249,-14.150522101063697]]]],\"type\":\"MultiPolygon\"},\"id\":\"3429\",\"properties\":{\"geographicLevel\":2,\"name\":\"MTI_84\",\"parentId\":\"2953\",\"status\":\"Active\",\"version\":0},\"serverVersion\":1545218281480,\"syncStatus\":\"Synced\",\"type\":\"Feature\"}";


    public static String structureJSON = "{\"id\": \"170230\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.5961026, -14.1715113]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3429\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626353}";

    public static String caseConfirmstionEventJSON = "{\"_id\":\"463e8cd4-acba-4f12-bbff-8435bced0227\",\"obs\":[],\"_rev\":\"v1\",\"type\":\"Event\",\"teamId\":\"09962f7c-8dab-4dee-96e2-354382aec76a\",\"details\":{\"family_name\":\"วีรศักดิ์\",\"focus_id\":\"2301110301\",\"focus_name\":\"ท่ากุ่มบน(ชายเขา)\",\"surname\":\"กัวติด\",\"first_name\":\"วีรศักดิ์\",\"age\":\"20\",\"case_number\":\"131412000001031181107101758977\",\"case_classification\":\"Bz\",\"focus_status\":\"B1\",\"focus_reason\":\"Investigation\",\"species\":\"V\",\"investigtion_date\":\"2018-11-06T00:00:00.000+0000\",\"ep1_create_date\":\"2018-11-07T10:10:27.673+0000\",\"ep3_create_date\":\"2018-11-07T10:17:58.977+0000\",\"house_number\":\"114\",\"plan_id\":\"10f9e9fa-ce34-4b27-a961-72fab5206ab6\"},\"version\":null,\"duration\":0,\"eventDate\":\"2019-01-08T09:14:32.807+02:00\",\"eventType\":\"Case Details\",\"entityType\":\"Case Details\",\"providerId\":\"nifi-user\",\"dateCreated\":\"2019-06-25T15:08:19.182+02:00\",\"identifiers\":{},\"baseEntityId\":\"bd73f7d7-4387-4b6b-b632-acb03c4ea160\",\"serverVersion\":null,\"formSubmissionId\":\"9c53270a-97f7-11e9-bc42-526af7764f64\"}";

    public static final String DUMMY_OPERATIONAL_AREA = "Akros_1";

    public static TaskDetails getTaskDetails() {
        TaskDetails taskDetails = new TaskDetails(UUID.randomUUID().toString());
        taskDetails.setDistanceFromUser(25.5f);
        taskDetails.setTaskStatus(TaskStatus.COMPLETED.name());
        taskDetails.setStructureName("Kenny House");
        taskDetails.setTaskCode(Intervention.IRS);
        taskDetails.setBusinessStatus(BusinessStatus.NOT_SPRAYABLE);
        taskDetails.setTaskEntity(UUID.randomUUID().toString());
        taskDetails.setLocation(new Location("Test"));
        return taskDetails;
    }


    public static StructureTaskDetails getStructureTaskDetails() {
        StructureTaskDetails taskDetails = new StructureTaskDetails(UUID.randomUUID().toString());
        taskDetails.setTaskStatus(TaskStatus.READY.name());
        taskDetails.setTaskCode(Intervention.REGISTER_FAMILY);
        taskDetails.setBusinessStatus(NOT_VISITED);
        taskDetails.setTaskEntity(UUID.randomUUID().toString());
        taskDetails.setTaskAction("Register Family");
        return taskDetails;
    }


    public static CommonPersonObjectClient getCommonPersonObjectClient() {
        HashMap<String, String> map = new HashMap<>();
        map.put(KEY.FIRST_NAME, "Charity");
        map.put(KEY.LAST_NAME, "Otala");
        map.put(KEY.DOB, "1982-01-01T03:00:00.000+03:00");
        map.put(KEY.GENDER, "Female");
        map.put(KEY.UNIQUE_ID, "12987632");
        CommonPersonObjectClient smartRegisterClient = new CommonPersonObjectClient(UUID.randomUUID().toString(), null, null);
        smartRegisterClient.setColumnmaps(map);
        return smartRegisterClient;
    }


    public static Task getTask(String entityId) {
        Task task = new Task();
        task.setIdentifier(UUID.randomUUID().toString());
        task.setBusinessStatus(BusinessStatus.IN_PROGRESS);
        task.setStatus(TaskStatus.COMPLETED);
        task.setCode(Intervention.CASE_CONFIRMATION);
        task.setForEntity(entityId);
        task.setGroupIdentifier("Akros_1_id");
        task.setStructureId("structure-id");
        return task;
    }

    public static MatrixCursor getTaskCursor(Task task ) {
        String[] COLUMNS = {"_id", STATUS, BUSINESS_STATUS,  CODE, FOR, GROUPID, STRUCTURE_ID};

        MatrixCursor cursor = new MatrixCursor(COLUMNS);

        cursor.addRow(new Object[]{task.getIdentifier(), task.getStatus().name(),
                task.getBusinessStatus(), task.getCode(), task.getForEntity(), task.getGroupIdentifier(), task.getStructureId()});
        return cursor;
    }

    public static OfflineMapModel getOfflineMapModel() {
        OfflineMapModel model = new OfflineMapModel();
        model.setOfflineMapStatus(DOWNLOADED);
        model.setLocation(TestingUtils.gson.fromJson(TestingUtils.operationalAreaGeoJSON, org.smartregister.domain.Location.class));
        return model;
    }
    public static Feature getStructure() {
        return Feature.fromJson(structureJSON);
    }

    public static TaskFilterParams getFilterParams() {
        Map<String, Set<String>> filters = new HashMap<>();
        filters.put(Constants.Filter.STATUS, new HashSet<>(Collections.singleton(NOT_VISITED)));
        filters.put(Constants.Filter.CODE, new HashSet<>(Collections.singleton(Intervention.IRS)));
        filters.put(Constants.Filter.INTERVENTION_UNIT, new HashSet<>(Collections.singleton(InterventionType.STRUCTURE)));
        return new TaskFilterParams("Status", filters);
    }

    public static org.smartregister.domain.Location getOperationalArea() {
        return gson.fromJson(operationalAreaGeoJSON, org.smartregister.domain.Location.class);
    }

    public static OfflineRegion createMockOfflineRegion() throws Exception {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put(METADATA_JSON_FIELD_REGION_NAME, DUMMY_OPERATIONAL_AREA);

        byte[] metadata = jsonObject.toString().getBytes("utf-8");

        final OfflineRegion offlineRegion = Mockito.mock(OfflineRegion.class);

        Mockito.when(offlineRegion.getMetadata())
                .thenReturn(metadata);

        return offlineRegion;
    }

    public static Pair<List<String>, Map<String, OfflineRegion>> getOfflineRegionInfo() throws Exception {
        List<String> offlineRegionNames = Collections.singletonList(DUMMY_OPERATIONAL_AREA);

        MapBoxOfflineQueueTask offlineQueueTask = getMapBoxOfflineQueueTask();

        Map<String, MapBoxOfflineQueueTask> offlineQueueTaskMap = new HashMap<>();
        offlineQueueTaskMap.put(DUMMY_OPERATIONAL_AREA, offlineQueueTask);

        return new Pair(offlineRegionNames, offlineQueueTaskMap);
    }

    public static MapBoxOfflineQueueTask getMapBoxOfflineQueueTask() throws Exception {
        JSONObject task = new JSONObject();
        task.put(METADATA_JSON_FIELD_REGION_NAME, DUMMY_OPERATIONAL_AREA);
        MapBoxOfflineQueueTask offlineQueueTask = new MapBoxOfflineQueueTask();
        offlineQueueTask.setTaskStatus(MapBoxOfflineQueueTask.TASK_STATUS_DONE);
        offlineQueueTask.setTaskType(MapBoxOfflineQueueTask.TASK_TYPE_DOWNLOAD);
        offlineQueueTask.setTask(task);
        return offlineQueueTask;
    }

    public static Map<String, MapBoxOfflineQueueTask> getOfflineQueueTaskMap() throws Exception {
        Map<String, MapBoxOfflineQueueTask> offlineQueueTaskMap = new HashMap<>();

        MapBoxOfflineQueueTask offlineQueueTask = getMapBoxOfflineQueueTask();
        offlineQueueTask.setDateCreated(new Date());
        offlineQueueTaskMap.put("location_1",offlineQueueTask);
        return offlineQueueTaskMap;
    }

}
