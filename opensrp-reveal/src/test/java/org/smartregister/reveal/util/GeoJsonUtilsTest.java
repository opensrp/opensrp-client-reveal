package org.smartregister.reveal.util;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.mapbox.geojson.Feature;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.junit.Before;
import org.junit.Test;
import org.smartregister.domain.Location;
import org.smartregister.domain.LocationProperty;
import org.smartregister.domain.Task;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.model.StructureDetails;
import org.smartregister.util.PropertiesConverter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.smartregister.domain.Task.TaskStatus;
import static org.smartregister.reveal.util.Constants.BusinessStatus.ADHERENCE_VISIT_DONE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BEDNET_DISTRIBUTED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BLOOD_SCREENING_COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FAMILY_REGISTERED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.FULLY_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.INCOMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.IN_PROGRESS;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NONE_RECEIVED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.PARTIALLY_RECEIVED;
import static org.smartregister.reveal.util.Constants.GeoJSON.IS_INDEX_CASE;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;
import static org.smartregister.reveal.util.Constants.Intervention.FI;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MDA;
import static org.smartregister.reveal.util.Constants.Intervention.MDA_ADHERENCE;
import static org.smartregister.reveal.util.Constants.Intervention.MDA_DISPENSE;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.Intervention.PAOT;
import static org.smartregister.reveal.util.Constants.Intervention.REGISTER_FAMILY;
import static org.smartregister.reveal.util.Constants.Properties.FAMILY_MEMBER_NAMES;
import static org.smartregister.reveal.util.Constants.Properties.STRUCTURE_NAME;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE;


public class GeoJsonUtilsTest extends BaseUnitTest {

    private String locationJSon = "{\"id\": \"3537\", \"type\": \"Feature\", \"geometry\": {\"type\": \"MultiPolygon\", \"coordinates\": [[[[32.64555352892119, -14.15491759447286], [32.64564603883843, -14.154955463350856], [32.6457355072855, -14.1549997471252], [32.64582146782072, -14.155050214924412], [32.64590347228988, -14.155106603636565], [32.64598109316318, -14.155168619281367], [32.64605392576265, -14.155235938542202], [32.64612159037363, -14.155308210452002], [32.64617583569173, -14.155375290811488], [32.64622989128036, -14.155433027025364], [32.64629203516403, -14.155509874795538], [32.646348334291865, -14.155590897782698], [32.64639849514264, -14.155675673574304], [32.646442256196444, -14.155763760192595], [32.64647938929766, -14.15585469839834], [32.64650970084471, -14.155948014084741], [32.64653303279953, -14.156043220749606], [32.64654926351215, -14.156139822031488], [32.64655830835385, -14.156237314297524], [32.64656012016035, -14.156335189268926], [32.6465546894762, -14.15643293667102], [32.64654668917796, -14.156494377451903], [32.64655058689322, -14.156536389971048], [32.64655239870205, -14.156634264939893], [32.6465497266393, -14.1566823595881], [32.6465573089099, -14.156695174161765], [32.646595732244286, -14.156730688660044], [32.64666339729779, -14.15680296056092], [32.6467255415539, -14.156879808322397], [32.6467818410191, -14.156960831300236], [32.64683200217058, -14.157045607082122], [32.64687576348677, -14.157133693690351], [32.64691289681058, -14.157224631885638], [32.646943208539305, -14.15731794756152], [32.64696654063419, -14.157413154215472], [32.64698277144395, -14.15750975548644], [32.64699181633994, -14.157607247741337], [32.6469936281573, -14.157705122701483], [32.64698819744063, -14.157802870092436], [32.646975552494204, -14.157899980304077], [32.646955759233485, -14.157995947047421], [32.646928920842974, -14.158090269994997], [32.646895177237745, -14.158182457387907], [32.64685470433414, -14.158272028601115], [32.64680771313299, -14.158358516648601], [32.6467544486191, -14.158441470617898], [32.64671978913156, -14.158487667998141], [32.64669202938966, -14.15856350737192], [32.64665155641858, -14.158653078582287], [32.64660456513888, -14.158739566626933], [32.646551300536366, -14.158822520593727], [32.64649204030376, -14.158901507994866], [32.64642709339352, -14.158976117022458], [32.6463567984075, -14.15904595869517], [32.646281521831035, -14.159110668886509], [32.64620165612251, -14.159169910222996], [32.64611761766684, -14.159223373843357], [32.646029844606055, -14.159270781009353], [32.6459387945527, -14.159311884557834], [32.64584494220565, -14.159346470190595], [32.64574877687465, -14.159374357591329], [32.645650799928994, -14.15939540136554], [32.64555152218349, -14.159409491798893], [32.64545146123515, -14.159416555429171], [32.645351138764845, -14.159416555429171], [32.645251077816674, -14.159409491798893], [32.64515180007117, -14.15939540136554], [32.645053823125515, -14.159374357591329], [32.644957657794514, -14.159346470190595], [32.644863805447464, -14.159311884557834], [32.64477275539411, -14.159270781009353], [32.6446849823331, -14.159223373843357], [32.644600943877656, -14.159169910222996], [32.64452107816913, -14.159110668886509], [32.64444580159249, -14.15904595869517], [32.64437550660647, -14.158976117022458], [32.6443105596964, -14.158901507994866], [32.6442512994638, -14.158822520593727], [32.64424031976625, -14.158805420879727], [32.64421108728816, -14.158766457170767], [32.64413733482177, -14.158711750306846], [32.64406205837661, -14.158647040113348], [32.64399176351315, -14.158577198438362], [32.64392681671642, -14.158502589408268], [32.64386755658716, -14.158423602004572], [32.643814292077586, -14.158340648035104], [32.643767300879965, -14.158254159987504], [32.64372682797937, -14.158164588774124], [32.643693084376984, -14.158072401381101], [32.64366624598858, -14.157978078433528], [32.64364645272945, -14.157882111689897], [32.643633807783765, -14.157785001478143], [32.64362837706761, -14.157687254087076], [32.64363018888479, -14.157589379126645], [32.6436392337801, -14.157491886871693], [32.64365546458867, -14.157395285600611], [32.64366824398308, -14.1573431392556], [32.6436550474292, -14.15733335047781], [32.64357977143788, -14.157268640277035], [32.64350947699847, -14.157198798593864], [32.64344453059339, -14.157124189555358], [32.64338527082139, -14.157045202142566], [32.643332006632924, -14.156962248163662], [32.64328501571867, -14.156875760106171], [32.64324454306216, -14.15678618888256], [32.64321079966316, -14.156694001478908], [32.643183961436584, -14.156599678520646], [32.64316416829666, -14.156503711766046], [32.64315152342731, -14.15640660154315], [32.64314609274367, -14.156308854140887], [32.64314790454983, -14.156210979169314], [32.64315694939079, -14.156113486903108], [32.64317318010125, -14.15601688562094], [32.64319651205346, -14.155921678955902], [32.64322682359715, -14.155828363269276], [32.643263956693936, -14.155737425063418], [32.643307717742736, -14.155649338444785], [32.64335787858783, -14.155564562653066], [32.643414177709126, -14.155483539665791], [32.6434763215858, -14.155406691895333], [32.64354398622594, -14.155334419985987], [32.64361681885685, -14.155267100725776], [32.64369443976357, -14.155205085081544], [32.64377644426809, -14.155148696369789], [32.643862404840256, -14.155098228571031], [32.64395187332587, -14.155053944796972], [32.644044383282896, -14.15501607591926], [32.64413945241409, -14.154984819365549], [32.6442365850799, -14.15496033809023], [32.64433527488432, -14.154942759725142], [32.644435007312715, -14.154932175914379], [32.64453526241561, -14.154928641835792], [32.6445440285737, -14.15492895085043], [32.64457177107891, -14.15491759447286], [32.64466684016889, -14.154886337918807], [32.64476397279315, -14.154861856643318], [32.644862662554935, -14.154844278278059], [32.6449623949403, -14.154833694467182], [32.64506265000005, -14.154830160388652], [32.64516290505974, -14.154833694467182], [32.64526263744511, -14.154844278278059], [32.64536132720689, -14.154861856643318], [32.645458459831154, -14.154886337918807], [32.64555352892119, -14.15491759447286]]]]}, \"properties\": {\"name\": \"MTI_13\", \"status\": \"Active\", \"version\": 0, \"parentId\": \"2953\", \"geographicLevel\": 2}, \"serverVersion\": 1542965231622}";
    private Map<String, StructureDetails> structureNames = new HashMap<>();


    @Before
    public void setUp() {
        PreferencesUtil.getInstance().setCurrentPlan("Focus 1");
        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", FI);
    }

    @Test
    public void testIsIndexCaseIsSetToTrueWhenTaskCodeIsCaseConfirmation() throws Exception {

        Location structure = initTestStructure();


        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Task task = initTestTask(null, null);

        tasks.put(structure.getId(), Collections.singleton(task));

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, structure.getId(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        boolean isIndexCase = feature.hasProperty(IS_INDEX_CASE) && feature.getBooleanProperty(IS_INDEX_CASE);
        assertTrue(isIndexCase);

    }

    @Test
    public void testIsIndexCaseIsNotSetToTrueWhenTaskCodeIsNotCaseConfirmation() throws Exception {

        Location structure = initTestStructure();


        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Task task = initTestTask(null, null);
        task.setCode(BEDNET_DISTRIBUTION); // Value set to another code that
        // is not "Case Confirmation"

        tasks.put(structure.getId(), Collections.singleton(task));

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        boolean isIndexCase = feature.hasProperty(IS_INDEX_CASE) && feature.getBooleanProperty(IS_INDEX_CASE);
        assertFalse(isIndexCase);

    }


    @Test
    public void testRegisterFamilyTaskIsPopulatedCorrectly() throws Exception {

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task task = initTestTask(null, null);
        task.setCode(REGISTER_FAMILY);
        taskSet.add(task);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(REGISTER_FAMILY, feature.getStringProperty(TASK_CODE));

    }


    @Test
    public void testCorrectTaskBusinessStatusIsSetForTaskColorCoding() throws Exception {

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task task = initTestTask(null, null);
        task.setCode(REGISTER_FAMILY);
        task.setBusinessStatus(COMPLETE);
        taskSet.add(task);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(REGISTER_FAMILY, feature.getStringProperty(TASK_CODE));
        assertEquals(FAMILY_REGISTERED, feature.getStringProperty(TASK_BUSINESS_STATUS));


    }


    @Test
    public void testCorrectTaskBusinessStatusIsSetForTasksCompleteColorCoding() throws Exception {

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task bednetDistributionTask = initTestTask(BEDNET_DISTRIBUTION, COMPLETE);
        taskSet.add(bednetDistributionTask);

        Task bloodScreeningTask = initTestTask(BLOOD_SCREENING, COMPLETE);
        taskSet.add(bloodScreeningTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(COMPLETE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForRegisterFamilyCompleteColorCoding() throws Exception {

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task bednetDistributionTask = initTestTask(BEDNET_DISTRIBUTION, IN_PROGRESS);
        taskSet.add(bednetDistributionTask);

        Task bloodScreeningTask = initTestTask(BLOOD_SCREENING, INCOMPLETE);
        taskSet.add(bloodScreeningTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(FAMILY_REGISTERED, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForBednetDistributionCompleteColorCoding() throws Exception {

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task bednetDistributionTask = initTestTask(BEDNET_DISTRIBUTION, COMPLETE);
        taskSet.add(bednetDistributionTask);

        Task bloodScreeningTask = initTestTask(BLOOD_SCREENING, INCOMPLETE);
        taskSet.add(bloodScreeningTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(BEDNET_DISTRIBUTED, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForBLoodScreeningCompleteColorCoding() throws Exception {

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task bednetDistributionTask = initTestTask(BEDNET_DISTRIBUTION, IN_PROGRESS);
        taskSet.add(bednetDistributionTask);

        Task bloodScreeningTask = initTestTask(BLOOD_SCREENING, COMPLETE);
        taskSet.add(bloodScreeningTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(BLOOD_SCREENING_COMPLETE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }


    @Test
    public void testCorrectTaskBusinessStatusBloodScreeningAndBedNetIneligible() throws Exception {

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task bednetDistributionTask = initTestTask(BEDNET_DISTRIBUTION, NOT_ELIGIBLE);
        taskSet.add(bednetDistributionTask);

        Task bloodScreeningTask = initTestTask(BLOOD_SCREENING, NOT_ELIGIBLE);
        taskSet.add(bloodScreeningTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(),new HashMap<>());

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(COMPLETE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForMDAFullyReceivedColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", MDA);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task mdaDispenseTask1 = initTestTask(MDA_DISPENSE, FULLY_RECEIVED);
        taskSet.add(mdaDispenseTask1);

        Task mdaDispenseTask2 = initTestTask(MDA_DISPENSE, FULLY_RECEIVED);
        taskSet.add(mdaDispenseTask2);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(FULLY_RECEIVED, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForMDAPartiallyReceivedColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", MDA);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task fullyDispensedTask = initTestTask(MDA_DISPENSE, FULLY_RECEIVED);
        taskSet.add(fullyDispensedTask);

        Task noneReceivedTask = initTestTask(MDA_DISPENSE, NONE_RECEIVED);
        taskSet.add(noneReceivedTask);

        Task notElligibleTask = initTestTask(MDA_DISPENSE, NOT_ELIGIBLE);
        taskSet.add(notElligibleTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(PARTIALLY_RECEIVED, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForMDANoneReceivedColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", MDA);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task noneReceivedTask = initTestTask(MDA_DISPENSE, NONE_RECEIVED);
        taskSet.add(noneReceivedTask);

        Task noneReceivedTask1 = initTestTask(MDA_DISPENSE, NONE_RECEIVED);
        taskSet.add(noneReceivedTask1);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(NONE_RECEIVED, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForNotElligibleColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", MDA);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task notElligibleTask = initTestTask(MDA_DISPENSE, NOT_ELIGIBLE);
        taskSet.add(notElligibleTask);

        Task notElligibleTask1 = initTestTask(MDA_DISPENSE, NOT_ELIGIBLE);
        taskSet.add(notElligibleTask1);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(NOT_ELIGIBLE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForMDAAdheredColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", MDA);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task notElligibleTask = initTestTask(MDA_DISPENSE, NOT_ELIGIBLE);
        taskSet.add(notElligibleTask);

        Task fullyReceivedTask = initTestTask(MDA_DISPENSE, FULLY_RECEIVED);
        taskSet.add(fullyReceivedTask);

        Task nonReceivedTask = initTestTask(MDA_DISPENSE, NONE_RECEIVED);
        taskSet.add(nonReceivedTask);

        Task adherenceTask = initTestTask(MDA_ADHERENCE, COMPLETE);
        taskSet.add(adherenceTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(ADHERENCE_VISIT_DONE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }


    @Test
    public void testCorrectTaskBusinessStatusIsSetForNotElligibleFamRegColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", FI);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, NOT_ELIGIBLE);
        taskSet.add(familyRegTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(NOT_ELIGIBLE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForCompletePAOTColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", FI);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task paotTask = initTestTask(PAOT, COMPLETE);
        taskSet.add(paotTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(COMPLETE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForNotEligiblePAOTColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", FI);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task paotTask = initTestTask(PAOT, NOT_ELIGIBLE);
        taskSet.add(paotTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(NOT_ELIGIBLE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForCompleteMosquitoCollectionColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", FI);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task mosquitoCollectionTask = initTestTask(MOSQUITO_COLLECTION, COMPLETE);
        taskSet.add(mosquitoCollectionTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(COMPLETE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForNotEligibleMosquitoCollectionColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", FI);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task mosquitoCollectionTask = initTestTask(MOSQUITO_COLLECTION, NOT_ELIGIBLE);
        taskSet.add(mosquitoCollectionTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(NOT_ELIGIBLE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForCompleteLarvalDippingColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", FI);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task larvalDippingTask = initTestTask(LARVAL_DIPPING, COMPLETE);
        taskSet.add(larvalDippingTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(COMPLETE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }


    @Test
    public void testCorrectTaskBusinessStatusIsSetForNotEligibleLarvalDippingColorCoding() throws Exception {

        PreferencesUtil.getInstance().setInterventionTypeForPlan("Focus 1", FI);

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task larvalDippingTask = initTestTask(LARVAL_DIPPING, NOT_ELIGIBLE);
        taskSet.add(larvalDippingTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(NOT_ELIGIBLE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    // Single task use-case tests

    @Test
    public void testCorrectTaskBusinessStatusIsSetForSingleTaskBednetDistributionCompleteColorCoding() throws Exception {

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task bednetDistributionTask = initTestTask(BEDNET_DISTRIBUTION, COMPLETE);
        taskSet.add(bednetDistributionTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(BEDNET_DISTRIBUTED, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testCorrectTaskBusinessStatusIsSetForSingleTaskBloodScreeningCompleteColorCoding() throws Exception {

        Location structure = initTestStructure();

        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);

        Map<String, Set<Task>> tasks = new HashMap<>();

        Set<Task> taskSet = new HashSet<>();
        Task familyRegTask = initTestTask(REGISTER_FAMILY, COMPLETE);
        taskSet.add(familyRegTask);

        Task bloodScreeningTask = initTestTask(BLOOD_SCREENING, COMPLETE);
        taskSet.add(bloodScreeningTask);

        tasks.put(structure.getId(), taskSet);

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, UUID.randomUUID().toString(), structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(BLOOD_SCREENING_COMPLETE, feature.getStringProperty(TASK_BUSINESS_STATUS));

    }

    @Test
    public void testHandleFamilyRegDoneInOtherPlan() throws JSONException {
        Location structure = initTestStructure();
        ArrayList<Location> structures = new ArrayList<Location>();
        structures.add(structure);
        StructureDetails structureDetails = new StructureDetails("John House", "John");
        structureNames.put(structure.getId(), structureDetails);
        Map<String, Set<Task>> tasks = new HashMap<>();

        String geoJsonString = GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, null, structureNames);

        JSONArray featuresJsonArray = new JSONArray(geoJsonString);

        Feature feature = Feature.fromJson(featuresJsonArray.get(0).toString());

        assertEquals(FAMILY_REGISTERED, feature.getStringProperty(TASK_BUSINESS_STATUS));
        assertEquals("John", feature.getStringProperty(FAMILY_MEMBER_NAMES));
        assertEquals("John House", feature.getStringProperty(STRUCTURE_NAME));
    }


    private Task initTestTask(String taskCode, String businessStatus) {
        Task task = new Task();
        task.setIdentifier("ARCHIVE_2019-04");
        String taskBusinessSTatus = StringUtils.isNotEmpty(businessStatus) ? businessStatus : IN_PROGRESS;
        task.setBusinessStatus(taskBusinessSTatus);
        task.setStatus(TaskStatus.IN_PROGRESS);
        String code = StringUtils.isNotEmpty(taskCode) ? taskCode : CASE_CONFIRMATION;
        task.setCode(code);
        return task;
    }

    private Location initTestStructure() {
        Gson gson = new GsonBuilder()
                .registerTypeAdapter(LocationProperty.class, new PropertiesConverter()).create();

        Location structure = gson.fromJson(locationJSon, Location.class);
        structure.setType(Constants.StructureType.RESIDENTIAL);
        LocationProperty locationProperty = new LocationProperty();
        locationProperty.setUid("uniquiid");
        locationProperty.setVersion(1);
        locationProperty.setType("Residential");
        structure.setProperties(locationProperty);
        return structure;
    }
}
