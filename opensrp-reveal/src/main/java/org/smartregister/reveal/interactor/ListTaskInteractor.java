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
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.domain.Campaign;
import org.smartregister.domain.Location;
import org.smartregister.domain.LocationProperty;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.EventClient;
import org.smartregister.domain.tag.FormTag;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.repository.CampaignRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.LocationRepository;
import org.smartregister.repository.StructureRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.ListTaskContract.PresenterCallBack;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.util.DateTimeTypeConverter;
import org.smartregister.util.JsonFormUtils;
import org.smartregister.util.PropertiesConverter;
import org.smartregister.util.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.smartregister.reveal.util.Constants.DETAILS;
import static org.smartregister.reveal.util.Constants.GeoJSON.FEATURES;
import static org.smartregister.reveal.util.Constants.GeoJSON.FEATURE_COLLECTION;
import static org.smartregister.reveal.util.Constants.GeoJSON.TYPE;
import static org.smartregister.reveal.util.Constants.METADATA;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_UUID;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_VERSION;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.Properties.TASK_STATUS;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;
import static org.smartregister.reveal.util.Constants.STRUCTURE;
import static org.smartregister.util.JsonFormUtils.ENTITY_ID;
import static org.smartregister.util.JsonFormUtils.getJSONObject;
import static org.smartregister.util.JsonFormUtils.getString;

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

    private EventClientRepository eventClientRepository;

    private RevealClientProcessor clientProcessor;

    public ListTaskInteractor(PresenterCallBack presenterCallBack) {
        this(new AppExecutors());
        this.presenterCallBack = presenterCallBack;
        campaignRepository = RevealApplication.getInstance().getCampaignRepository();
        taskRepository = RevealApplication.getInstance().getTaskRepository();
        structureRepository = RevealApplication.getInstance().getStructureRepository();
        locationRepository = RevealApplication.getInstance().getLocationRepository();
        eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
        clientProcessor = RevealClientProcessor.getInstance(RevealApplication.getInstance().getApplicationContext());
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
                                taskProperties.put(TASK_CODE, task.getCode());
                                taskProperties.put(LOCATION_UUID, structure.getProperties().getUid());
                                taskProperties.put(LOCATION_VERSION, structure.getProperties().getVersion() + "");
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

    public void saveSprayForm(String json) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    JSONObject jsonForm = new JSONObject(json);
                    String entityId = getString(jsonForm, ENTITY_ID);
                    JSONArray fields = JsonFormUtils.fields(jsonForm);
                    JSONObject metadata = getJSONObject(jsonForm, METADATA);
                    FormTag formTag = new FormTag();
                    AllSharedPreferences sharedPreferences = RevealApplication.getInstance().getContext().allSharedPreferences();
                    formTag.providerId = sharedPreferences.fetchRegisteredANM();
                    formTag.locationId = sharedPreferences.fetchDefaultLocalityId(formTag.providerId);
                    formTag.teamId = sharedPreferences.fetchDefaultTeamId(formTag.providerId);
                    formTag.team = sharedPreferences.fetchDefaultTeam(formTag.providerId);
                    Event event = JsonFormUtils.createEvent(fields, metadata, formTag, entityId, SPRAY_EVENT, STRUCTURE);
                    JSONObject eventJson = new JSONObject(gson.toJson(event));
                    eventJson.put(DETAILS, getJSONObject(jsonForm, DETAILS));
                    org.smartregister.domain.db.Event dbEvent = gson.fromJson(eventJson.toString(), org.smartregister.domain.db.Event.class);


                    eventClientRepository.addEvent(entityId, eventJson);
                    List<EventClient> unprocessedEvents = new ArrayList<>();
                    unprocessedEvents.add(new EventClient(dbEvent, null));
                    clientProcessor.processClient(unprocessedEvents);

                    appExecutors.mainThread().execute(new Runnable() {
                        @Override
                        public void run() {
                            String businessStatus = clientProcessor.calculateBusinessStatus(dbEvent);
                            presenterCallBack.onSprayFormSaved(event.getBaseEntityId(), dbEvent.getDetails().get(TASK_IDENTIFIER),
                                    Task.TaskStatus.COMPLETED, businessStatus);
                        }
                    });
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
        };

        appExecutors.diskIO().execute(runnable);
    }
}
