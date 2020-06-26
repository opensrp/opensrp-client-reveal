package org.smartregister.reveal.util;

import android.location.Location;
import androidx.core.util.Pair;

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
import org.smartregister.domain.form.FormLocation;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Constants.InterventionType;
import org.smartregister.util.DateTimeTypeConverter;

import java.util.ArrayList;
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


    public static String structureJSON = "{\"id\": \"170230\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.5961026, -14.1715113]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3429\", \"geographicLevel\": 4, \"taskIdentifier\": \"5cc3f6f6-532b-11ea-8d77-2e728ce88125\", \"taskCode\": \"RACD Register Family\"}, \"serverVersion\": 1542970626353}";

    public static String caseConfirmstionEventJSON = "{\"_id\":\"463e8cd4-acba-4f12-bbff-8435bced0227\",\"obs\":[],\"_rev\":\"v1\",\"type\":\"Event\",\"teamId\":\"09962f7c-8dab-4dee-96e2-354382aec76a\",\"details\":{\"family_name\":\"วีรศักดิ์\",\"focus_id\":\"2301110301\",\"focus_name\":\"ท่ากุ่มบน(ชายเขา)\",\"surname\":\"กัวติด\",\"first_name\":\"วีรศักดิ์\",\"age\":\"20\",\"case_number\":\"131412000001031181107101758977\",\"case_classification\":\"Bz\",\"focus_status\":\"B1\",\"focus_reason\":\"Investigation\",\"species\":\"V\",\"investigtion_date\":\"2018-11-06T00:00:00.000+0000\",\"ep1_create_date\":\"2018-11-07T10:10:27.673+0000\",\"ep3_create_date\":\"2018-11-07T10:17:58.977+0000\",\"house_number\":\"114\",\"plan_id\":\"10f9e9fa-ce34-4b27-a961-72fab5206ab6\"},\"version\":null,\"duration\":0,\"eventDate\":\"2019-01-08T09:14:32.807+02:00\",\"eventType\":\"Case Details\",\"entityType\":\"Case Details\",\"providerId\":\"nifi-user\",\"dateCreated\":\"2019-06-25T15:08:19.182+02:00\",\"identifiers\":{},\"baseEntityId\":\"bd73f7d7-4387-4b6b-b632-acb03c4ea160\",\"serverVersion\":null,\"formSubmissionId\":\"9c53270a-97f7-11e9-bc42-526af7764f64\"}";

    public static String bloodScreeningEventJSON = "{\"identifiers\":{},\"baseEntityId\":\"c97d8968-2daa-42a7-99a8-c6e99c47ae3a\",\"locationId\":\"3951\",\"eventDate\":\"2020-03-10T23:00:00.000+02:00\",\"eventType\":\"blood_screening\",\"formSubmissionId\":\"5c0c87a0-4597-4e64-9c3d-d198c7cd4013\",\"providerId\":\"onatest\",\"duration\":0,\"obs\":[{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"eligiblePerson\",\"parentCode\":\"\",\"values\":[\"1\"],\"set\":[],\"formSubmissionField\":\"eligiblePerson\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"caseTestingProtocol\",\"parentCode\":\"\",\"values\":[\"RACD\\/CIS\"],\"set\":[],\"formSubmissionField\":\"caseTestingProtocol\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"forestGoerYesNo\",\"parentCode\":\"\",\"values\":[\"Yes\"],\"set\":[],\"formSubmissionField\":\"forestGoerYesNo\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"personTested\",\"parentCode\":\"\",\"values\":[\"Yes\"],\"set\":[],\"formSubmissionField\":\"personTested\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"testType\",\"parentCode\":\"\",\"values\":[\"RDT\"],\"set\":[],\"formSubmissionField\":\"testType\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"testRdtResult\",\"parentCode\":\"\",\"values\":[\"Negative\"],\"set\":[],\"formSubmissionField\":\"testRdtResult\",\"humanReadableValues\":[]},{\"fieldType\":\"formsubmissionField\",\"fieldDataType\":\"text\",\"fieldCode\":\"business_status\",\"parentCode\":\"\",\"values\":[\"Complete\"],\"set\":[],\"formSubmissionField\":\"business_status\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"start\",\"fieldCode\":\"163137AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"2020-03-11 19:01:15\"],\"set\":[],\"formSubmissionField\":\"start\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"end\",\"fieldCode\":\"163138AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"2020-03-11 19:01:28\"],\"set\":[],\"formSubmissionField\":\"end\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"deviceid\",\"fieldCode\":\"163149AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"358240051111110\"],\"set\":[],\"formSubmissionField\":\"deviceid\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"subscriberid\",\"fieldCode\":\"163150AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"310260000000000\"],\"set\":[],\"formSubmissionField\":\"subscriberid\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"simserial\",\"fieldCode\":\"163151AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"89014103211118510720\"],\"set\":[],\"formSubmissionField\":\"simserial\",\"humanReadableValues\":[]},{\"fieldType\":\"concept\",\"fieldDataType\":\"phonenumber\",\"fieldCode\":\"163152AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"parentCode\":\"\",\"values\":[\"+15555215554\"],\"set\":[],\"formSubmissionField\":\"phonenumber\",\"humanReadableValues\":[]}],\"entityType\":\"ec_family_member\",\"details\":{\"taskStatus\":\"READY\",\"location_id\":\"c0879dd5-bb6c-4e6f-bddc-627648517d5e\",\"form_version\":\"0.0.1\",\"locationUUID\":\"e4d6f668-3590-41ff-89fb-2ede9bec048f\",\"appVersionName\":\"3.3.1\",\"taskIdentifier\":\"dc1b2963-a69c-4ec6-a222-5a1e5f0fd254\",\"locationVersion\":\"0\",\"taskBusinessStatus\":\"Not Visited\"},\"version\":1583942488191,\"teamId\":\"52fe5128-1384-566e-922d-1d10bdb356d3\",\"team\":\"Akros_Test\",\"dateCreated\":\"2020-03-11T18:05:13.872+02:00\",\"serverVersion\":1583942713848,\"clientApplicationVersion\":17,\"clientDatabaseVersion\":4,\"type\":\"Event\",\"id\":\"cffbfbb8-bc35-4884-8425-de6f35446757\",\"revision\":\"v1\"}";

    public static final String DUMMY_JSON_FORM_STRING = "{\r\n  \"count\": \"1\",\r\n  \"encounter_type\": \"Birth Registration\",\r\n  \"mother\": {\r\n    \"encounter_type\": \"New Woman Registration\"\r\n  },\r\n  \"entity_id\": \"\",\r\n  \"relational_id\": \"\",\r\n  \"metadata\": {\r\n    \"start\": {\r\n      \"openmrs_entity_parent\": \"\",\r\n      \"openmrs_entity\": \"concept\",\r\n      \"openmrs_data_type\": \"start\",\r\n      \"openmrs_entity_id\": \"163137AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\"\r\n    }\r\n  },\r\n  \"step1\": {\r\n    \"title\": \"Birth Registration\",\r\n    \"fields\": [\r\n      {\r\n        \"key\": \"Child_Photo\",\r\n        \"openmrs_entity_parent\": \"\",\r\n        \"openmrs_entity\": \"\",\r\n        \"openmrs_entity_id\": \"\",\r\n        \"type\": \"choose_image\",\r\n        \"uploadButtonText\": \"Take a photo of the child\"\r\n      },\r\n      {\r\n        \"key\": \"gps\",\r\n        \"openmrs_entity_parent\": \"usual_residence\",\r\n        \"openmrs_entity\": \"person_address\",\r\n        \"openmrs_entity_id\": \"geopoint\",\r\n        \"openmrs_data_type\": \"text\",\r\n        \"type\": \"gps\"\r\n      },\r\n      {\r\n        \"key\": \"Home_Facility\",\r\n        \"openmrs_entity_parent\": \"\",\r\n        \"openmrs_entity\": \"\",\r\n        \"openmrs_entity_id\": \"\",\r\n        \"openmrs_data_type\": \"text\",\r\n        \"type\": \"tree\",\r\n        \"hint\": \"Child's home health facility *\",\r\n        \"tree\": [],\r\n        \"v_required\": {\r\n          \"value\": true,\r\n          \"err\": \"Please enter the child's home facility\"\r\n        }\r\n      }\r\n    ]\r\n  }\r\n}";

    public static String feature = "{\"geometry\":{\"coordinates\":[[[101.1761078,15.0666717],[101.1762902,15.0665732],[101.1762151,15.066467],[101.1760381,15.0665603],[101.1761078,15.0666717]]],\"type\":\"Polygon\"},\"id\":\"5d609281-6784-415e-9eee-8806c204b58c\",\"properties\":{\"is_index_case\":true,\"geographicLevel\":5.0,\"parentId\":\"450fc15b-5bd2-468a-927a-49cb10d3bcac\",\"taskStatus\":\"COMPLETED\",\"version\":0.0,\"taskCode\":\"Bednet Distribution\",\"status\":\"Active\",\"locationVersion\":\"0\",\"taskBusinessStatus\":\"Complete\",\"taskIdentifier\":\"c987a804-2525-43bd-99b1-e1910fffbc1a\"},\"type\":\"Feature\"}";

    public static String familyRegJSON = "{\"count\":\"2\",\"encounter_type\":\"Family Registration\",\"entity_id\":\"\",\"relational_id\":\"\",\"metadata\":{\"start\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"start\",\"openmrs_entity_id\":\"163137AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"2020-04-16 15:20:23\"},\"end\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"end\",\"openmrs_entity_id\":\"163138AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"2020-04-16 15:20:53\"},\"today\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"encounter\",\"openmrs_entity_id\":\"encounter_date\",\"value\":\"16-04-2020\"},\"deviceid\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"deviceid\",\"openmrs_entity_id\":\"163149AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"358240051111110\"},\"subscriberid\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"subscriberid\",\"openmrs_entity_id\":\"163150AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"310260000000000\"},\"simserial\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"simserial\",\"openmrs_entity_id\":\"163151AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"89014103211118510720\"},\"phonenumber\":{\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_data_type\":\"phonenumber\",\"openmrs_entity_id\":\"163152AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"value\":\"+15555215554\"},\"encounter_location\":\"\",\"look_up\":{\"entity_id\":\"\",\"value\":\"\"}},\"step1\":{\"title\":\"Family details\",\"next\":\"step2\",\"fields\":[{\"key\":\"fam_name\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"first_name\",\"type\":\"edit_text\",\"hint\":\"First name of Head of Household\",\"edit_type\":\"name\",\"v_required\":{\"value\":\"true\",\"err\":\"Please enter first name of Head of Household\"},\"value\":\"John\",\"step\":\"step1\",\"is-rule-check\":true},{\"key\":\"house_number\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person_address\",\"openmrs_entity_id\":\"address2\",\"type\":\"edit_text\",\"hint\":\"House Number\",\"value\":\"\"},{\"key\":\"street\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person_address\",\"openmrs_entity_id\":\"street\",\"type\":\"edit_text\",\"hint\":\"Street\",\"value\":\"\"},{\"key\":\"landmark\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person_address\",\"openmrs_entity_id\":\"landmark\",\"type\":\"edit_text\",\"hint\":\"Landmark\",\"value\":\"\"}]},\"step2\":{\"title\":\"Family head\",\"fields\":[{\"key\":\"unique_id\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person_identifier\",\"openmrs_entity_id\":\"opensrp_id\",\"type\":\"edit_text\",\"hint\":\"ID\",\"read_only\":\"True\",\"v_required\":{\"value\":\"true\",\"err\":\"Please enter the ID\"},\"value\":\"23592173\"},{\"key\":\"first_name\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"edit_text\",\"hint\":\"First name\",\"edit_type\":\"name\",\"v_required\":{\"value\":\"true\",\"err\":\"Please enter first name of Head of Household\"},\"relevance\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-register-relevance.yml\"}}},\"step\":\"step2\",\"is-rule-check\":true,\"is_visible\":false},{\"key\":\"same_as_fam_name\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"check_box\",\"label\":\"\",\"options\":[{\"key\":\"same_as_fam_name\",\"text\":\"First name same as household name\",\"text_size\":\"18px\",\"value\":true}],\"step\":\"step2\",\"is-rule-check\":true,\"value\":[\"same_as_fam_name\"]},{\"key\":\"surname\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"last_name\",\"type\":\"edit_text\",\"hint\":\"Surname\",\"edit_type\":\"name\",\"v_required\":{\"value\":\"true\",\"err\":\"Please enter the surname\"},\"value\":\"Doe\"},{\"key\":\"first_name_calculation\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"first_name\",\"type\":\"hidden\",\"calculation\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-register-calculation.yml\"}}},\"value\":\"John\"},{\"key\":\"dob\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"birthdate\",\"type\":\"date_picker\",\"hint\":\"Date of birth (DOB)\",\"expanded\":false,\"duration\":{\"label\":\"Age\"},\"min_date\":\"today-120y\",\"max_date\":\"today-5y\",\"v_required\":{\"value\":\"true\",\"err\":\"Please enter the date of birth\"},\"relevance\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-register-relevance.yml\"}}},\"is_visible\":true,\"value\":\"16-04-2011\"},{\"key\":\"dob_unknown\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"birthdateApprox\",\"type\":\"check_box\",\"label\":\"\",\"options\":[{\"key\":\"dob_unknown\",\"text\":\"DOB unknown?\",\"text_size\":\"18px\",\"value\":\"false\"}],\"step\":\"step2\",\"is-rule-check\":true},{\"key\":\"age\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person_attribute\",\"openmrs_entity_id\":\"age_entered\",\"type\":\"edit_text\",\"hint\":\"Age\",\"v_numeric_integer\":{\"value\":\"true\",\"err\":\"Please enter a number\"},\"v_min\":{\"value\":\"5\",\"err\":\"Age must be equal or greater than 5\"},\"v_max\":{\"value\":\"120\",\"err\":\"Age must be equal or less than 120\"},\"relevance\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-register-relevance.yml\"}}},\"v_required\":{\"value\":true,\"err\":\"Please enter the age\"},\"is_visible\":false,\"step\":\"step2\",\"is-rule-check\":true},{\"key\":\"sex\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"person\",\"openmrs_entity_id\":\"gender\",\"type\":\"native_radio\",\"label\":\"Sex\",\"options\":[{\"key\":\"Male\",\"text\":\"Male\"},{\"key\":\"Female\",\"text\":\"Female\"}],\"v_required\":{\"value\":\"true\",\"err\":\"Please enter the sex\"},\"value\":\"Male\"},{\"key\":\"phone_number\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_entity_id\":\"159635AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"type\":\"edit_text\",\"hint\":\"Phone number\",\"v_numeric\":{\"value\":\"true\",\"err\":\"Please enter a number\"},\"v_regex\":{\"value\":\"^$|0[0-9]{8,9}\",\"err\":\"Number must be 9-10 digits and must start with 0.\"},\"value\":\"\"},{\"key\":\"sleeps_outdoors\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"native_radio\",\"label\":\"Does this person spend the night outdoors?\",\"options\":[{\"key\":\"Yes\",\"text\":\"Yes\"},{\"key\":\"No\",\"text\":\"No\"}],\"v_required\":{\"value\":\"true\",\"err\":\"Please select whether this person spends the night outdoors\"},\"value\":\"No\"},{\"key\":\"occupation\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"edit_text\",\"hint\":\"Person's Occupation\",\"relevance\":{\"rules-engine\":{\"ex-rules\":{\"rules-file\":\"family-register-relevance.yml\"}}},\"is_visible\":false},{\"key\":\"citizenship\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"native_radio\",\"label\":\"Citizenship\",\"options\":[{\"key\":\"Thai\",\"text\":\"Thai\"},{\"key\":\"Migrant-1\",\"text\":\"Migrant 1\"},{\"key\":\"Migrant-2\",\"text\":\"Migrant 2\"}],\"v_required\":{\"value\":true,\"err\":\"Please specify the Citizenship\"},\"is-rule-check\":false,\"value\":\"Migrant-1\"},{\"key\":\"national_id\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"concept\",\"openmrs_entity_id\":\"163084AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"label_info_text\":\"What is their national identity number or their voter registration number?\",\"label_info_title\":\"National ID number\",\"type\":\"edit_text\",\"hint\":\"National ID number\",\"v_numeric\":{\"value\":\"true\",\"err\":\"Please enter a number\"},\"v_regex\":{\"value\":\"^$|[0-9]{13}\",\"err\":\"Number must be 13 digits.\"},\"relevance\":{\"step2:citizenship\":{\"type\":\"string\",\"ex\":\"equalTo(., \\\"Thai\\\")\"}},\"is_visible\":false}]},\"invisible_required_fields\":\"[first_name, age]\"}";

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

    public static FormLocation generateLocationHierarchy() {
        FormLocation facilityFormLocation = new FormLocation();
        facilityFormLocation.name = "Chadiza 1";
        facilityFormLocation.nodes = null;
        FormLocation districtFormLocation = new FormLocation();
        districtFormLocation.name = "Chadiza RHC";
        ArrayList<FormLocation> facilityFormLocations = new ArrayList<>();
        facilityFormLocations.add(facilityFormLocation);
        districtFormLocation.nodes = facilityFormLocations;
        FormLocation provinceFormLocation = new FormLocation();
        provinceFormLocation.name = "Lusaka";
        ArrayList<FormLocation> districtFormLocations = new ArrayList<>();
        districtFormLocations.add(facilityFormLocation);
        provinceFormLocation.nodes = districtFormLocations;
        FormLocation countryFormLocation= new FormLocation();
        countryFormLocation.name = "Zambia";
        ArrayList<FormLocation> provinceFormLocations = new ArrayList<>();
        provinceFormLocations.add(facilityFormLocation);
        countryFormLocation.nodes = provinceFormLocations;

        return countryFormLocation;
    }

}
