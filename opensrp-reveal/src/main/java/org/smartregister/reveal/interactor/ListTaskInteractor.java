package org.smartregister.reveal.interactor;

import android.support.annotation.VisibleForTesting;
import android.util.Log;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.joda.time.DateTime;
import org.joda.time.LocalDate;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.domain.Campaign;
import org.smartregister.domain.Location;
import org.smartregister.domain.LocationProperty;
import org.smartregister.domain.Task;
import org.smartregister.repository.CampaignRepository;
import org.smartregister.repository.LocationRepository;
import org.smartregister.repository.StructureRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.ListTaskContract.PresenterCallBack;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.util.DateTimeTypeConverter;
import org.smartregister.util.DateTypeConverter;
import org.smartregister.util.PropertiesConverter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.Properties.TASK_STATUS;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public class ListTaskInteractor {
    private static final Gson gson = new GsonBuilder().setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
            .registerTypeAdapter(DateTime.class, new DateTimeTypeConverter())
            .registerTypeAdapter(LocationProperty.class, new PropertiesConverter()).create();

    private static final String TAG = ListTaskInteractor.class.getCanonicalName();

    private AppExecutors appExecutors;

    private CampaignRepository campaignRepository;

    private TaskRepository taskRepository;

    private StructureRepository structureRepository;

    private LocationRepository locationRepository;

    private PresenterCallBack presenterCallBack;

    public ListTaskInteractor(PresenterCallBack presenterCallBack) {
        this(new AppExecutors());
        this.presenterCallBack = presenterCallBack;
        campaignRepository = RevealApplication.getInstance().getCampaignRepository();
        taskRepository = RevealApplication.getInstance().getTaskRepository();
        structureRepository = RevealApplication.getInstance().getStructureRepository();
        locationRepository = RevealApplication.getInstance().getLocationRepository();
    }

    @VisibleForTesting
    protected ListTaskInteractor(AppExecutors appExecutors) {
        this.appExecutors = appExecutors;
    }

    public void fetchCampaigns() {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                //TODO remove method call once sync is implemented
                generateDummyCampaign();
                List<Campaign> campaigns = campaignRepository.getAllCampaigns();
                appExecutors.mainThread().execute(new Runnable() {
                    @Override
                    public void run() {
                        presenterCallBack.onCampaignsFetched(campaigns);
                    }
                });

            }
        };

        appExecutors.diskIO().execute(runnable);
    }


    public void fetchLocations(String campaign, String operationalArea) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                final JSONObject featureCollection = new JSONObject();
                Location operationalAreaLocation = locationRepository.getLocationByName(operationalArea);
                if (operationalAreaLocation != null) {
                    Map<String, Task> tasks = taskRepository.getTasksByCampaignAndGroup(campaign, operationalAreaLocation.getId());
                    List<Location> structures = structureRepository.getLocationsByParentId(operationalAreaLocation.getId());
                    for (Location structure : structures) {
                        Task task = tasks.get(structure.getId());
                        if (task != null) {
                            HashMap<String, String> taskProperties = new HashMap<>();
                            taskProperties.put(TASK_IDENTIFIER, task.getIdentifier());
                            taskProperties.put(TASK_BUSINESS_STATUS, task.getBusinessStatus());
                            taskProperties.put(TASK_STATUS, task.getStatus().name());
                            structure.getProperties().setCustomProperties(taskProperties);
                        }
                    }

                    if (!structures.isEmpty()) {
                        try {
                            featureCollection.put("type", "FeatureCollection");
                            featureCollection.put("features", new JSONArray(gson.toJson(structures)));
                        } catch (JSONException e) {
                            Log.e(TAG, e.getMessage(), e);
                        }
                    }
                }
                appExecutors.mainThread().execute(new Runnable() {
                    @Override
                    public void run() {
                        presenterCallBack.onStructuresFetched(featureCollection);
                    }
                });
            }
        };

        appExecutors.diskIO().execute(runnable);
    }


    /**
     * TODO remove once sync is implemented
     */
    private void generateDummyCampaign() {
        try {
            Gson gson = new GsonBuilder().registerTypeAdapter(DateTime.class, new DateTimeTypeConverter("yyyy-MM-dd'T'HHmm"))
                    .registerTypeAdapter(LocalDate.class, new DateTypeConverter())
                    .create();
            String campaignJson = "{\"identifier\":\"IRS_2018_S1\",\"title\":\"2019 IRS Season 1\",\"description\":\"This is the 2010 IRS Spray Campaign for Zambia for the first spray season dated 1 Jan 2019 - 31 Mar 2019.\",\"status\":\"In Progress\",\"executionPeriod\":{\"start\":\"2019-01-01\",\"end\":\"2019-03-31\"},\"authoredOn\":\"2018-10-01T0900\",\"lastModified\":\"2018-10-01T0900\",\"owner\":\"jdoe\",\"serverVersion\":0}";
            String campaign2Json = "{\"identifier\":\"IRS_2018_S2\",\"title\":\"2019 IRS Season 2\",\"description\":\"This is the 2010 IRS Spray Campaign for Zambia for the second spray season dated 1 Jan 2019 - 31 Mar 2019.\",\"status\":\"In Progress\",\"executionPeriod\":{\"start\":\"2019-01-01\",\"end\":\"2019-03-31\"},\"authoredOn\":\"2018-10-01T0900\",\"lastModified\":\"2018-10-01T0900\",\"owner\":\"jdoe\",\"serverVersion\":0}";

            Campaign campaign = gson.fromJson(campaignJson, Campaign.class);
            campaignRepository.addOrUpdate(campaign);

            Campaign campaign2 = gson.fromJson(campaign2Json, Campaign.class);
            campaignRepository.addOrUpdate(campaign2);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
