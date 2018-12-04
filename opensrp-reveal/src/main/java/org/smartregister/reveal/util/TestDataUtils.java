package org.smartregister.reveal.util;

import android.content.SharedPreferences;
import android.preference.PreferenceManager;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;

import org.joda.time.DateTime;
import org.joda.time.LocalDate;
import org.smartregister.domain.Campaign;
import org.smartregister.domain.Location;
import org.smartregister.domain.LocationProperty;
import org.smartregister.domain.Task;
import org.smartregister.repository.CampaignRepository;
import org.smartregister.repository.LocationRepository;
import org.smartregister.repository.StructureRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.util.DateTimeTypeConverter;
import org.smartregister.util.DateTypeConverter;
import org.smartregister.util.PropertiesConverter;

import java.util.List;

/**
 * Created by samuelgithengi on 12/3/18.
 */
public class TestDataUtils {

    private CampaignRepository campaignRepository;

    private TaskRepository taskRepository;

    private LocationRepository locationRepository;

    private StructureRepository structureRepository;

    private String TEST_DATA_POPULATED = "test.data.populated";


    public TestDataUtils() {
        campaignRepository = RevealApplication.getInstance().getCampaignRepository();
        taskRepository = RevealApplication.getInstance().getTaskRepository();
        locationRepository = RevealApplication.getInstance().getLocationRepository();
        structureRepository = RevealApplication.getInstance().getStructureRepository();
    }

    public void populateTestData() {
        SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(RevealApplication.getInstance().getApplicationContext());
        if (!sharedPreferences.getBoolean(TEST_DATA_POPULATED, false)) {
            createCampaigns();
            createTasks();
            createLocations();
            createStructures();
            sharedPreferences.edit().putBoolean(TEST_DATA_POPULATED, true).apply();
        }
    }

    private void createCampaigns() {
        try {
            Gson gson = new GsonBuilder().registerTypeAdapter(DateTime.class, new DateTimeTypeConverter("yyyy-MM-dd'T'HHmm"))
                    .registerTypeAdapter(LocalDate.class, new DateTypeConverter())
                    .create();
            String campaignJson = "{\"title\": \"2019 IRS Season 1\", \"status\": \"In Progress\", \"identifier\": \"IRS_2019_S1\", \"description\": \"This is the 2019 IRS Spray Campaign for Zambia for the first spray season dated 1 Jan 2019 - 31 Mar 2019.\", \"serverVersion\": 1543451954837, \"executionPeriod\": {\"end\": \"2019-03-31\", \"start\": \"2019-01-01\"}}";
            String campaign2Json = "{\"identifier\":\"IRS_2018_S2\",\"title\":\"2019 IRS Season 2\",\"description\":\"This is the 2010 IRS Spray Campaign for Zambia for the second spray season dated 1 Jan 2019 - 31 Mar 2019.\",\"status\":\"In Progress\",\"executionPeriod\":{\"start\":\"2019-01-01\",\"end\":\"2019-03-31\"},\"authoredOn\":\"2018-10-01T0900\",\"lastModified\":\"2018-10-01T0900\",\"owner\":\"jdoe\",\"serverVersion\":0}";

            Campaign campaign = gson.fromJson(campaignJson, Campaign.class);
            campaignRepository.addOrUpdate(campaign);

            Campaign campaign2 = gson.fromJson(campaign2Json, Campaign.class);
            campaignRepository.addOrUpdate(campaign2);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    private void createTasks() {
        try {

            Gson gson = new GsonBuilder().registerTypeAdapter(DateTime.class, new DateTimeTypeConverter("yyyy-MM-dd'T'HH:mm:ss.SSSZ"))
                    .serializeNulls().create();
            String tasksJSON = "[{\"for\": \"156718\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:38:22.380+02:00\", \"identifier\": \"31b3db65-732a-40f9-8f77-162b20cdf6bc\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:38:22.380+02:00\", \"serverVersion\": 1543455616943, \"businessStatus\": \"Not Visited\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156718\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.106+02:00\", \"identifier\": \"83783f77-4c8c-4bc9-b80d-0bcd420f17a4\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.106+02:00\", \"serverVersion\": 1543455736957, \"businessStatus\": \"Not Visited\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156748\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.138+02:00\", \"identifier\": \"70aaa481-b11e-4246-838f-b85afb41efc2\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.138+02:00\", \"serverVersion\": 1543455736958, \"businessStatus\": \"Not Visited\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156732\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.169+02:00\", \"identifier\": \"c6dd4abc-fb3e-4f72-afb8-923fc43f44d7\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.169+02:00\", \"serverVersion\": 1543455736959, \"businessStatus\": \"Not Visited\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156729\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.200+02:00\", \"identifier\": \"2caa810d-d4da-4e67-838b-badb9bd86e06\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.200+02:00\", \"serverVersion\": 1543455736960, \"businessStatus\": \"Not Sprayable\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156722\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.234+02:00\", \"identifier\": \"340718da-6eb2-4b51-a98c-d76b24ff2dd7\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.234+02:00\", \"serverVersion\": 1543455736961, \"businessStatus\": \"Not Visited\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156742\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.265+02:00\", \"identifier\": \"f704c1bc-806f-46df-927b-e88ad380f8a7\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.265+02:00\", \"serverVersion\": 1543455736962, \"businessStatus\": \"Not Visited\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156736\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.297+02:00\", \"identifier\": \"230b5be9-1ccd-4191-9b4d-b486612d861d\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.297+02:00\", \"serverVersion\": 1543455736963, \"businessStatus\": \"Not Sprayed\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156774\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.328+02:00\", \"identifier\": \"c68b461c-7ca8-4de2-ae2c-5e6e661519b7\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.328+02:00\", \"serverVersion\": 1543455736964, \"businessStatus\": \"Not Sprayed\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156758\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.360+02:00\", \"identifier\": \"6d5c0305-3c28-4a2e-ac16-6dde80e51809\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.360+02:00\", \"serverVersion\": 1543455736965, \"businessStatus\": \"Sprayed\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156755\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.392+02:00\", \"identifier\": \"076885f8-582e-4dc6-8a1a-510e1c8ed5d9\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.392+02:00\", \"serverVersion\": 1543455736966, \"businessStatus\": \"Not Visited\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156727\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.421+02:00\", \"identifier\": \"634fa9fa-736d-4298-96aa-3de68ac02cae\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.421+02:00\", \"serverVersion\": 1543455736967, \"businessStatus\": \"Not Visited\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"},\n" +
                    " {\"for\": \"156763\", \"code\": \"IRS\", \"focus\": \"IRS Visit\", \"owner\": \"demoMTI\", \"status\": \"Ready\", \"priority\": 3, \"authoredOn\": \"2018-11-29T03:42:03.452+02:00\", \"identifier\": \"d3b237ff-f9d8-4077-9523-c7bf3552ff87\", \"description\": \"Spray House\", \"lastModified\": \"2018-11-29T03:42:03.452+02:00\", \"serverVersion\": 1543455736968, \"businessStatus\": \"Not Visited\", \"groupIdentifier\": \"3537\", \"campaignIdentifier\": \"IRS_2019_S1\", \"executionStartDate\": \"2018-11-10T22:00:00.000+02:00\"}]";
            List<Task> tasks = gson.fromJson(tasksJSON, new TypeToken<List<Task>>() {
            }.getType());
            for (Task task : tasks) {
                try {
                    taskRepository.addOrUpdate(task);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    private void createLocations() {
        try {
            Gson gson = new GsonBuilder().setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
                    .registerTypeAdapter(DateTime.class, new DateTimeTypeConverter())
                    .registerTypeAdapter(LocationProperty.class, new PropertiesConverter()).create();
            String locationJSon = "{\"id\": \"3537\", \"type\": \"Feature\", \"geometry\": {\"type\": \"MultiPolygon\", \"coordinates\": [[[[32.64555352892119, -14.15491759447286], [32.64564603883843, -14.154955463350856], [32.6457355072855, -14.1549997471252], [32.64582146782072, -14.155050214924412], [32.64590347228988, -14.155106603636565], [32.64598109316318, -14.155168619281367], [32.64605392576265, -14.155235938542202], [32.64612159037363, -14.155308210452002], [32.64617583569173, -14.155375290811488], [32.64622989128036, -14.155433027025364], [32.64629203516403, -14.155509874795538], [32.646348334291865, -14.155590897782698], [32.64639849514264, -14.155675673574304], [32.646442256196444, -14.155763760192595], [32.64647938929766, -14.15585469839834], [32.64650970084471, -14.155948014084741], [32.64653303279953, -14.156043220749606], [32.64654926351215, -14.156139822031488], [32.64655830835385, -14.156237314297524], [32.64656012016035, -14.156335189268926], [32.6465546894762, -14.15643293667102], [32.64654668917796, -14.156494377451903], [32.64655058689322, -14.156536389971048], [32.64655239870205, -14.156634264939893], [32.6465497266393, -14.1566823595881], [32.6465573089099, -14.156695174161765], [32.646595732244286, -14.156730688660044], [32.64666339729779, -14.15680296056092], [32.6467255415539, -14.156879808322397], [32.6467818410191, -14.156960831300236], [32.64683200217058, -14.157045607082122], [32.64687576348677, -14.157133693690351], [32.64691289681058, -14.157224631885638], [32.646943208539305, -14.15731794756152], [32.64696654063419, -14.157413154215472], [32.64698277144395, -14.15750975548644], [32.64699181633994, -14.157607247741337], [32.6469936281573, -14.157705122701483], [32.64698819744063, -14.157802870092436], [32.646975552494204, -14.157899980304077], [32.646955759233485, -14.157995947047421], [32.646928920842974, -14.158090269994997], [32.646895177237745, -14.158182457387907], [32.64685470433414, -14.158272028601115], [32.64680771313299, -14.158358516648601], [32.6467544486191, -14.158441470617898], [32.64671978913156, -14.158487667998141], [32.64669202938966, -14.15856350737192], [32.64665155641858, -14.158653078582287], [32.64660456513888, -14.158739566626933], [32.646551300536366, -14.158822520593727], [32.64649204030376, -14.158901507994866], [32.64642709339352, -14.158976117022458], [32.6463567984075, -14.15904595869517], [32.646281521831035, -14.159110668886509], [32.64620165612251, -14.159169910222996], [32.64611761766684, -14.159223373843357], [32.646029844606055, -14.159270781009353], [32.6459387945527, -14.159311884557834], [32.64584494220565, -14.159346470190595], [32.64574877687465, -14.159374357591329], [32.645650799928994, -14.15939540136554], [32.64555152218349, -14.159409491798893], [32.64545146123515, -14.159416555429171], [32.645351138764845, -14.159416555429171], [32.645251077816674, -14.159409491798893], [32.64515180007117, -14.15939540136554], [32.645053823125515, -14.159374357591329], [32.644957657794514, -14.159346470190595], [32.644863805447464, -14.159311884557834], [32.64477275539411, -14.159270781009353], [32.6446849823331, -14.159223373843357], [32.644600943877656, -14.159169910222996], [32.64452107816913, -14.159110668886509], [32.64444580159249, -14.15904595869517], [32.64437550660647, -14.158976117022458], [32.6443105596964, -14.158901507994866], [32.6442512994638, -14.158822520593727], [32.64424031976625, -14.158805420879727], [32.64421108728816, -14.158766457170767], [32.64413733482177, -14.158711750306846], [32.64406205837661, -14.158647040113348], [32.64399176351315, -14.158577198438362], [32.64392681671642, -14.158502589408268], [32.64386755658716, -14.158423602004572], [32.643814292077586, -14.158340648035104], [32.643767300879965, -14.158254159987504], [32.64372682797937, -14.158164588774124], [32.643693084376984, -14.158072401381101], [32.64366624598858, -14.157978078433528], [32.64364645272945, -14.157882111689897], [32.643633807783765, -14.157785001478143], [32.64362837706761, -14.157687254087076], [32.64363018888479, -14.157589379126645], [32.6436392337801, -14.157491886871693], [32.64365546458867, -14.157395285600611], [32.64366824398308, -14.1573431392556], [32.6436550474292, -14.15733335047781], [32.64357977143788, -14.157268640277035], [32.64350947699847, -14.157198798593864], [32.64344453059339, -14.157124189555358], [32.64338527082139, -14.157045202142566], [32.643332006632924, -14.156962248163662], [32.64328501571867, -14.156875760106171], [32.64324454306216, -14.15678618888256], [32.64321079966316, -14.156694001478908], [32.643183961436584, -14.156599678520646], [32.64316416829666, -14.156503711766046], [32.64315152342731, -14.15640660154315], [32.64314609274367, -14.156308854140887], [32.64314790454983, -14.156210979169314], [32.64315694939079, -14.156113486903108], [32.64317318010125, -14.15601688562094], [32.64319651205346, -14.155921678955902], [32.64322682359715, -14.155828363269276], [32.643263956693936, -14.155737425063418], [32.643307717742736, -14.155649338444785], [32.64335787858783, -14.155564562653066], [32.643414177709126, -14.155483539665791], [32.6434763215858, -14.155406691895333], [32.64354398622594, -14.155334419985987], [32.64361681885685, -14.155267100725776], [32.64369443976357, -14.155205085081544], [32.64377644426809, -14.155148696369789], [32.643862404840256, -14.155098228571031], [32.64395187332587, -14.155053944796972], [32.644044383282896, -14.15501607591926], [32.64413945241409, -14.154984819365549], [32.6442365850799, -14.15496033809023], [32.64433527488432, -14.154942759725142], [32.644435007312715, -14.154932175914379], [32.64453526241561, -14.154928641835792], [32.6445440285737, -14.15492895085043], [32.64457177107891, -14.15491759447286], [32.64466684016889, -14.154886337918807], [32.64476397279315, -14.154861856643318], [32.644862662554935, -14.154844278278059], [32.6449623949403, -14.154833694467182], [32.64506265000005, -14.154830160388652], [32.64516290505974, -14.154833694467182], [32.64526263744511, -14.154844278278059], [32.64536132720689, -14.154861856643318], [32.645458459831154, -14.154886337918807], [32.64555352892119, -14.15491759447286]]]]}, \"properties\": {\"name\": \"MTI_13\", \"status\": \"Active\", \"version\": 0, \"parentId\": \"2953\", \"geographicLevel\": 2}, \"serverVersion\": 1542965231622}";
            Location location = gson.fromJson(locationJSon, Location.class);
            locationRepository.addOrUpdate(location);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    private void createStructures() {

        try {
            Gson gson = new GsonBuilder().setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
                    .registerTypeAdapter(DateTime.class, new DateTimeTypeConverter())
                    .registerTypeAdapter(LocationProperty.class, new PropertiesConverter()).create();
            String locationJSon = "[{\"id\": \"156742\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.6454013, -14.1580617]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626312},\n" +
                    " {\"id\": \"156755\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.64560445, -14.15768065]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626316},\n" +
                    " {\"id\": \"156732\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.64506265, -14.1561859]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626308},\n" +
                    " {\"id\": \"156748\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.645017555116304, -14.1576627813855]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626317},\n" +
                    " {\"id\": \"156736\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.64536072686922, -14.157382556100632]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626313},\n" +
                    " {\"id\": \"156722\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.644948962863104, -14.156475398638218]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626310},\n" +
                    " {\"id\": \"156729\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.64494836063221, -14.156382876801207]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626309},\n" +
                    " {\"id\": \"156763\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.64533055, -14.15779445]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626319},\n" +
                    " {\"id\": \"156727\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.64517095032837, -14.156310716564187]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626318},\n" +
                    " {\"id\": \"156718\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.64516322705264, -14.156609792235928]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626311},\n" +
                    " {\"id\": \"156774\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.6453312, -14.1579658]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626314},\n" +
                    " {\"id\": \"156758\", \"type\": \"Feature\", \"geometry\": {\"type\": \"Point\", \"coordinates\": [32.64544453303698, -14.157812316402199]}, \"properties\": {\"status\": \"Active\", \"version\": 0, \"parentId\": \"3537\", \"geographicLevel\": 4}, \"serverVersion\": 1542970626315}]";
            List<Location> structures = gson.fromJson(locationJSon, new TypeToken<List<Location>>() {
            }.getType());
            for (Location structure : structures) {
                try {
                    structureRepository.addOrUpdate(structure);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
