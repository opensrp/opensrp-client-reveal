package org.smartregister.reveal.interactor;

import android.support.annotation.VisibleForTesting;
import android.util.Log;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.mapbox.geojson.Geometry;

import org.joda.time.DateTime;
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
import org.smartregister.util.PropertiesConverter;
import org.smartregister.util.Utils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.smartregister.reveal.util.Constants.GeoJSON.FEATURES;
import static org.smartregister.reveal.util.Constants.GeoJSON.FEATURE_COLLECTION;
import static org.smartregister.reveal.util.Constants.GeoJSON.TYPE;
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
                final JSONObject featureCollection = createFutureCollection();
                Location operationalAreaLocation = locationRepository.getLocationByName(operationalArea);
                try {
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
                        if (!Utils.isEmptyCollection(structures)) {
                            featureCollection.put(FEATURES, new JSONArray(gson.toJson(structures)));
                            Geometry.fromJson(gson.toJson(operationalAreaLocation.getGeometry()));
                            Log.d(TAG, "features:" + featureCollection.toString());
                        }
                    }
                } catch (Exception e) {
                    Log.e(TAG, e.getMessage(), e);
                }
                appExecutors.mainThread().execute(new Runnable() {
                    @Override
                    public void run() {
                        Geometry geometry = operationalAreaLocation == null ? null : Geometry.fromJson(gson.toJson(operationalAreaLocation.getGeometry()));
                        presenterCallBack.onStructuresFetched(featureCollection, geometry);
                    }
                });
            }
        };

        appExecutors.diskIO().execute(runnable);
    }

    private JSONObject createFutureCollection() {
        JSONObject featureCollection = new JSONObject();
        try {
            featureCollection.put(TYPE, FEATURE_COLLECTION);
        } catch (JSONException e) {
            Log.e(TAG, "Error creating feature collection");
            return null;
        }
        return featureCollection;
    }

}
