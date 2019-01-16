package org.smartregister.reveal.interactor;

import android.content.Context;
import android.support.annotation.VisibleForTesting;
import android.util.Log;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.mapbox.geojson.Feature;
import com.mapbox.geojson.Geometry;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

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
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.CampaignRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.LocationRepository;
import org.smartregister.repository.StructureRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.ListTaskContract.PresenterCallBack;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.GeoJsonUtils;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;
import org.smartregister.util.DateTimeTypeConverter;
import org.smartregister.util.JsonFormUtils;
import org.smartregister.util.PropertiesConverter;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static com.cocoahero.android.geojson.Geometry.JSON_COORDINATES;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.DETAILS;
import static org.smartregister.reveal.util.Constants.GeoJSON.FEATURES;
import static org.smartregister.reveal.util.Constants.GeoJSON.FEATURE_COLLECTION;
import static org.smartregister.reveal.util.Constants.GeoJSON.TYPE;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.JsonForm.ENCOUNTER_TYPE;
import static org.smartregister.reveal.util.Constants.JsonForm.IRS_VISIT;
import static org.smartregister.reveal.util.Constants.JsonForm.RESIDENTIAL;
import static org.smartregister.reveal.util.Constants.METADATA;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_TYPE;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_UUID;
import static org.smartregister.reveal.util.Constants.Properties.LOCATION_VERSION;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.Properties.TASK_STATUS;
import static org.smartregister.reveal.util.Constants.REGISTER_STRUCTURE_EVENT;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;
import static org.smartregister.reveal.util.Constants.STRUCTURE;
import static org.smartregister.util.JsonFormUtils.ENTITY_ID;
import static org.smartregister.util.JsonFormUtils.getJSONObject;
import static org.smartregister.util.JsonFormUtils.getString;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public class ListTaskInteractor {
    public static final Gson gson = new GsonBuilder().setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
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

    private String operationalAreaId;

    private AllSharedPreferences sharedPreferences;

    public ListTaskInteractor(PresenterCallBack presenterCallBack) {
        this(new AppExecutors());
        this.presenterCallBack = presenterCallBack;
        campaignRepository = RevealApplication.getInstance().getCampaignRepository();
        taskRepository = RevealApplication.getInstance().getTaskRepository();
        structureRepository = RevealApplication.getInstance().getStructureRepository();
        locationRepository = RevealApplication.getInstance().getLocationRepository();
        eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
        clientProcessor = RevealClientProcessor.getInstance(RevealApplication.getInstance().getApplicationContext());
        sharedPreferences = RevealApplication.getInstance().getContext().allSharedPreferences();
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

    public void fetchCardViewDetails(String structureId) {
        final String sql = "SELECT spray_status, not_sprayed_reason, not_sprayed_other_reason, property_type, spray_date," +
                " spray_operator, family_head_name FROM sprayed_structures WHERE id=?";
        SQLiteDatabase db = RevealApplication.getInstance().getRepository().getWritableDatabase();
        Cursor cursor = db.rawQuery(sql, new String[]{structureId});
        CardDetails cardDetails = null;
        try {
            if (cursor.moveToFirst()) {
                cardDetails = createCardDetails(cursor);
            }
        } catch (Exception e) {
            Log.e(TAG, Log.getStackTraceString(e));
        } finally {
            if (cursor != null) {
                cursor.close();
            }
        }

        CardDetails finalCardDetails = cardDetails;
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                appExecutors.mainThread().execute(new Runnable() {
                    @Override
                    public void run() {
                        presenterCallBack.onCardDetailsFetched(finalCardDetails);
                    }
                });
            }
        };
        appExecutors.diskIO().execute(runnable);
    }

    private CardDetails createCardDetails(Cursor cursor) {
        String reason = cursor.getString(cursor.getColumnIndex("not_sprayed_reason"));
        if ("other".equals(reason)) {
            reason = cursor.getString(cursor.getColumnIndex("not_sprayed_other_reason"));
        }
        return new CardDetails(
                cursor.getString(cursor.getColumnIndex("spray_status")),
                cursor.getString(cursor.getColumnIndex("property_type")),
                cursor.getString(cursor.getColumnIndex("spray_date")),
                cursor.getString(cursor.getColumnIndex("spray_operator")),
                cursor.getString(cursor.getColumnIndex("family_head_name")),
                reason
        );
    }

    public void fetchLocations(String campaign, String operationalArea) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                final JSONObject featureCollection = createFeatureCollection();
                Location operationalAreaLocation = Utils.getOperationalAreaLocation(operationalArea);
                try {
                    if (operationalAreaLocation != null) {
                        Map<String, Task> tasks = taskRepository.getTasksByCampaignAndGroup(campaign, operationalAreaLocation.getId());
                        List<Location> structures = structureRepository.getLocationsByParentId(operationalAreaLocation.getId());
                        featureCollection.put(FEATURES, new JSONArray(GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks)));
                        Log.d(TAG, "features:" + featureCollection.toString());

                    }
                } catch (Exception e) {
                    Log.e(TAG, e.getMessage(), e);
                }
                appExecutors.mainThread().execute(new Runnable() {
                    @Override
                    public void run() {
                        if (operationalAreaLocation != null) {
                            Geometry geometry = Geometry.fromJson(gson.toJson(operationalAreaLocation.getGeometry()));
                            operationalAreaId = operationalAreaLocation.getId();
                            presenterCallBack.onStructuresFetched(featureCollection, geometry);
                        } else {
                            presenterCallBack.onStructuresFetched(featureCollection, null);
                        }
                    }
                });
            }
        };

        appExecutors.diskIO().execute(runnable);
    }

    private JSONObject createFeatureCollection() {
        JSONObject featureCollection = new JSONObject();
        try {
            featureCollection.put(TYPE, FEATURE_COLLECTION);
        } catch (JSONException e) {
            Log.e(TAG, "Error creating feature collection");
            return null;
        }
        return featureCollection;
    }


    public void saveJsonForm(String json) {
        try {
            JSONObject jsonForm = new JSONObject(json);
            String encounterType = jsonForm.getString(ENCOUNTER_TYPE);
            if (SPRAY_EVENT.equals(encounterType))
                saveSprayForm(jsonForm);
            else if (REGISTER_STRUCTURE_EVENT.equals(encounterType))
                saveRegisterStructureForm(jsonForm);
        } catch (Exception e) {
            Log.e(TAG, "Error saving Json Form data", e);
        }
    }


    private org.smartregister.domain.db.Event saveEvent(JSONObject jsonForm) throws JSONException {
        String entityId = getString(jsonForm, ENTITY_ID);
        JSONArray fields = JsonFormUtils.fields(jsonForm);
        JSONObject metadata = getJSONObject(jsonForm, METADATA);
        FormTag formTag = new FormTag();
        formTag.providerId = sharedPreferences.fetchRegisteredANM();
        formTag.locationId = sharedPreferences.fetchDefaultLocalityId(formTag.providerId);
        formTag.teamId = sharedPreferences.fetchDefaultTeamId(formTag.providerId);
        formTag.team = sharedPreferences.fetchDefaultTeam(formTag.providerId);
        Event event = JsonFormUtils.createEvent(fields, metadata, formTag, entityId, SPRAY_EVENT, STRUCTURE);
        JSONObject eventJson = new JSONObject(gson.toJson(event));
        eventJson.put(DETAILS, getJSONObject(jsonForm, DETAILS));
        eventClientRepository.addEvent(entityId, eventJson);
        return gson.fromJson(eventJson.toString(), org.smartregister.domain.db.Event.class);


    }

    private void saveSprayForm(JSONObject jsonForm) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    org.smartregister.domain.db.Event event = saveEvent(jsonForm);
                    clientProcessor.processClient(Collections.singletonList(new EventClient(event, null)), true);
                    appExecutors.mainThread().execute(new Runnable() {
                        @Override
                        public void run() {
                            String businessStatus = clientProcessor.calculateBusinessStatus(event);
                            presenterCallBack.onSprayFormSaved(event.getBaseEntityId(), event.getDetails().get(TASK_IDENTIFIER),
                                    Task.TaskStatus.COMPLETED, businessStatus);
                        }
                    });
                } catch (JSONException e) {
                    Log.e(TAG, "Error saving spraye", e);
                    presenterCallBack.onFormSaveFailure(SPRAY_EVENT);
                }
            }
        };

        appExecutors.diskIO().execute(runnable);
    }

    private void saveRegisterStructureForm(JSONObject jsonForm) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    jsonForm.put(ENTITY_ID, UUID.randomUUID().toString());
                    org.smartregister.domain.db.Event event = saveEvent(jsonForm);
                    com.cocoahero.android.geojson.Feature feature = new com.cocoahero.android.geojson.Feature(new JSONObject(event.findObs(null, false, "structure").getValue().toString()));
                    Location structure = new Location();
                    structure.setId(event.getBaseEntityId());
                    structure.setType(feature.getType());
                    org.smartregister.domain.Geometry geometry = new org.smartregister.domain.Geometry();
                    geometry.setType(org.smartregister.domain.Geometry.GeometryType.valueOf(feature.getGeometry().getType().toUpperCase()));
                    JsonArray coordinates = new JsonArray();
                    JSONArray featureCoordinates = feature.getGeometry().toJSON().getJSONArray(JSON_COORDINATES);
                    coordinates.add(featureCoordinates.get(0).toString());
                    coordinates.add(featureCoordinates.get(1).toString());
                    geometry.setCoordinates(coordinates);
                    structure.setGeometry(geometry);
                    LocationProperty properties = new LocationProperty();
                    properties.setType(RESIDENTIAL);
                    properties.setEffectiveStartDate(new DateTime());
                    properties.setParentId(operationalAreaId);
                    properties.setStatus(LocationProperty.PropertyStatus.PENDING_REVIEW);
                    properties.setUid(UUID.randomUUID().toString());
                    structure.setProperties(properties);
                    structure.setSyncStatus(BaseRepository.TYPE_Created);
                    structureRepository.addOrUpdate(structure);

                    Task task = new Task();
                    task.setIdentifier(UUID.randomUUID().toString());
                    task.setCampaignIdentifier(PreferencesUtil.getInstance().getCurrentCampaignId());
                    task.setGroupIdentifier(operationalAreaId);
                    task.setStatus(Task.TaskStatus.READY);
                    task.setBusinessStatus(NOT_VISITED);
                    task.setPriority(3);
                    task.setCode(IRS);
                    Context applicationContext = RevealApplication.getInstance().getApplicationContext();
                    task.setDescription(applicationContext.getString(R.string.irs_task_description));
                    task.setFocus(IRS_VISIT);
                    task.setForEntity(structure.getId());
                    task.setAuthoredOn(new DateTime());
                    task.setLastModified(new DateTime());
                    task.setOwner(event.getProviderId());
                    task.setSyncStatus(BaseRepository.TYPE_Created);
                    taskRepository.addOrUpdate(task);
                    appExecutors.mainThread().execute(new Runnable() {
                        @Override
                        public void run() {
                            Map<String, String> taskProperties = new HashMap<>();
                            taskProperties.put(TASK_IDENTIFIER, task.getIdentifier());
                            taskProperties.put(TASK_BUSINESS_STATUS, task.getBusinessStatus());
                            taskProperties.put(TASK_STATUS, task.getStatus().name());
                            taskProperties.put(TASK_CODE, task.getCode());
                            taskProperties.put(LOCATION_UUID, structure.getProperties().getUid());
                            taskProperties.put(LOCATION_VERSION, structure.getProperties().getVersion() + "");
                            taskProperties.put(LOCATION_TYPE, structure.getProperties().getType());
                            structure.getProperties().setCustomProperties(taskProperties);
                            presenterCallBack.onStructureAdded(Feature.fromJson(gson.toJson(structure)), featureCoordinates);
                        }
                    });
                } catch (JSONException e) {
                    Log.e(TAG, "Error saving new Structure", e);
                    presenterCallBack.onFormSaveFailure(REGISTER_STRUCTURE_EVENT);
                }
            }
        };

        appExecutors.diskIO().execute(runnable);
    }
}
