package org.smartregister.reveal.interactor;

import android.content.Context;
import android.util.Log;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.mapbox.geojson.Feature;

import org.joda.time.DateTime;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.domain.Location;
import org.smartregister.domain.LocationProperty;
import org.smartregister.domain.Task;
import org.smartregister.domain.db.Client;
import org.smartregister.domain.db.EventClient;
import org.smartregister.domain.tag.FormTag;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.StructureRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseContract;
import org.smartregister.reveal.contract.BaseContract.BasePresenter;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Constants.JsonForm;
import org.smartregister.reveal.util.Constants.Properties;
import org.smartregister.reveal.util.Constants.StructureType;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.util.DateTimeTypeConverter;
import org.smartregister.util.JsonFormUtils;
import org.smartregister.util.PropertiesConverter;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static com.cocoahero.android.geojson.Geometry.JSON_COORDINATES;
import static org.smartregister.reveal.util.Constants.BEDNET_DISTRIBUTION_EVENT;
import static org.smartregister.reveal.util.Constants.BLOOD_SCREENING_EVENT;
import static org.smartregister.reveal.util.Constants.CASE_CONFIRMATION_EVENT;
import static org.smartregister.reveal.util.Constants.DETAILS;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.METADATA;
import static org.smartregister.reveal.util.Constants.MOSQUITO_COLLECTION_EVENT;
import static org.smartregister.reveal.util.Constants.REGISTER_STRUCTURE_EVENT;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;
import static org.smartregister.reveal.util.Constants.STRUCTURE;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY_MEMBER;
import static org.smartregister.util.JsonFormUtils.ENTITY_ID;
import static org.smartregister.util.JsonFormUtils.getJSONObject;
import static org.smartregister.util.JsonFormUtils.getString;


/**
 * Created by samuelgithengi on 3/25/19.
 */
public abstract class BaseInteractor implements BaseContract.BaseInteractor {

    private static final String TAG = BaseInteractor.class.getCanonicalName();

    public static final Gson gson = new GsonBuilder().setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
            .registerTypeAdapter(DateTime.class, new DateTimeTypeConverter())
            .registerTypeAdapter(LocationProperty.class, new PropertiesConverter()).create();


    protected TaskRepository taskRepository;

    protected StructureRepository structureRepository;

    protected BasePresenter presenterCallBack;

    protected String operationalAreaId;

    protected AppExecutors appExecutors;

    private AllSharedPreferences sharedPreferences;

    private EventClientRepository eventClientRepository;

    private RevealClientProcessor clientProcessor;


    public BaseInteractor(BasePresenter presenterCallBack) {
        this.presenterCallBack = presenterCallBack;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        taskRepository = RevealApplication.getInstance().getTaskRepository();
        structureRepository = RevealApplication.getInstance().getStructureRepository();
        eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
        clientProcessor = RevealClientProcessor.getInstance(RevealApplication.getInstance().getApplicationContext());
        sharedPreferences = RevealApplication.getInstance().getContext().allSharedPreferences();

    }

    @Override
    public void saveJsonForm(String json) {
        try {
            JSONObject jsonForm = new JSONObject(json);
            String encounterType = jsonForm.getString(JsonForm.ENCOUNTER_TYPE);
            if (SPRAY_EVENT.equals(encounterType)) {
                saveSprayForm(jsonForm);
            } else if (REGISTER_STRUCTURE_EVENT.equals(encounterType)) {
                saveRegisterStructureForm(jsonForm);
            } else if (MOSQUITO_COLLECTION_EVENT.equals(encounterType)) {
                saveMosquitoCollectionForm(jsonForm);
            } else if (BEDNET_DISTRIBUTION_EVENT.equals(encounterType)) {
                saveBednetDistributionForm(jsonForm);
            } else if (BLOOD_SCREENING_EVENT.equals(encounterType)) {
                saveMemberForm(jsonForm, encounterType, Intervention.BLOOD_SCREENING);
            } else if (CASE_CONFIRMATION_EVENT.equals(encounterType)) {
                saveMemberForm(jsonForm, encounterType, Intervention.CASE_CONFIRMATION);
            }
        } catch (Exception e) {
            Log.e(TAG, "Error saving Json Form data", e);
        }
    }

    private org.smartregister.domain.db.Event saveEvent(JSONObject jsonForm, String encounterType, String bindType) throws JSONException {
        String entityId = getString(jsonForm, ENTITY_ID);
        JSONArray fields = JsonFormUtils.fields(jsonForm);
        JSONObject metadata = getJSONObject(jsonForm, METADATA);
        FormTag formTag = new FormTag();
        formTag.providerId = sharedPreferences.fetchRegisteredANM();
        formTag.locationId = sharedPreferences.fetchDefaultLocalityId(formTag.providerId);
        formTag.teamId = sharedPreferences.fetchDefaultTeamId(formTag.providerId);
        formTag.team = sharedPreferences.fetchDefaultTeam(formTag.providerId);
        Event event = JsonFormUtils.createEvent(fields, metadata, formTag, entityId, encounterType, bindType);
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
                    org.smartregister.domain.db.Event event = saveEvent(jsonForm, SPRAY_EVENT, STRUCTURE);
                    clientProcessor.processClient(Collections.singletonList(new EventClient(event, null)), true);
                    appExecutors.mainThread().execute(new Runnable() {
                        @Override
                        public void run() {
                            String businessStatus = clientProcessor.calculateBusinessStatus(event);
                            presenterCallBack.onFormSaved(event.getBaseEntityId(),
                                    Task.TaskStatus.COMPLETED, businessStatus, Intervention.IRS);

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
                    jsonForm.put(DETAILS, new JSONObject().put(Properties.LOCATION_PARENT, operationalAreaId));
                    org.smartregister.domain.db.Event event = saveEvent(jsonForm, REGISTER_STRUCTURE_EVENT, STRUCTURE);
                    com.cocoahero.android.geojson.Feature feature = new com.cocoahero.android.geojson.Feature(new JSONObject(event.findObs(null, false, "structure").getValue().toString()));
                    DateTime now = new DateTime();
                    Location structure = new Location();
                    structure.setId(event.getBaseEntityId());
                    structure.setType(feature.getType());
                    org.smartregister.domain.Geometry geometry = new org.smartregister.domain.Geometry();
                    geometry.setType(org.smartregister.domain.Geometry.GeometryType.valueOf(feature.getGeometry().getType().toUpperCase()));
                    JsonArray coordinates = new JsonArray();
                    JSONArray featureCoordinates = feature.getGeometry().toJSON().getJSONArray(JSON_COORDINATES);
                    coordinates.add(Double.parseDouble(featureCoordinates.get(0).toString()));
                    coordinates.add(Double.parseDouble(featureCoordinates.get(1).toString()));
                    geometry.setCoordinates(coordinates);
                    structure.setGeometry(geometry);
                    LocationProperty properties = new LocationProperty();
                    String structureType = event.findObs(null, false, JsonForm.STRUCTURE_TYPE).getValue().toString();
                    properties.setType(structureType);
                    properties.setEffectiveStartDate(now);
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
                    task.setBusinessStatus(BusinessStatus.NOT_VISITED);
                    task.setPriority(3);
                    Context applicationContext = RevealApplication.getInstance().getApplicationContext();
                    if (StructureType.RESIDENTIAL.equals(structureType)) {
                        task.setCode(Intervention.IRS);
                        task.setDescription(applicationContext.getString(R.string.irs_task_description));
                        task.setFocus(Intervention.IRS_VISIT);
                    } else if (StructureType.MOSQUITO_COLLECTION_POINT.equals(structureType)) {
                        task.setCode(Intervention.MOSQUITO_COLLECTION);
                        task.setDescription(applicationContext.getString(R.string.mosquito_collection_task_description));
                        task.setFocus(Intervention.MOSQUITO_COLLECTION);
                    } else if (StructureType.LARVAL_BREEDING_SITE.equals(structureType)) {
                        task.setCode(Intervention.LARVAL_DIPPING);
                        task.setDescription(applicationContext.getString(R.string.larval_dipping_task_description));
                        task.setFocus(Constants.Intervention.LARVAL_DIPPING);
                    }
                    task.setForEntity(structure.getId());
                    task.setExecutionStartDate(now);
                    task.setAuthoredOn(now);
                    task.setLastModified(now);
                    task.setOwner(event.getProviderId());
                    task.setSyncStatus(BaseRepository.TYPE_Created);
                    taskRepository.addOrUpdate(task);
                    clientProcessor.processClient(Collections.singletonList(new EventClient(event, null)), true);
                    appExecutors.mainThread().execute(new Runnable() {
                        @Override
                        public void run() {
                            Map<String, String> taskProperties = new HashMap<>();
                            taskProperties.put(Constants.Properties.TASK_IDENTIFIER, task.getIdentifier());
                            taskProperties.put(Properties.TASK_BUSINESS_STATUS, task.getBusinessStatus());
                            taskProperties.put(Properties.TASK_STATUS, task.getStatus().name());
                            taskProperties.put(Properties.TASK_CODE, task.getCode());
                            taskProperties.put(Properties.LOCATION_UUID, structure.getProperties().getUid());
                            taskProperties.put(Properties.LOCATION_VERSION, structure.getProperties().getVersion() + "");
                            taskProperties.put(Properties.LOCATION_TYPE, structure.getProperties().getType());
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

    private void saveMosquitoCollectionForm(JSONObject jsonForm) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    org.smartregister.domain.db.Event event = saveEvent(jsonForm, MOSQUITO_COLLECTION_EVENT, STRUCTURE);
                    clientProcessor.processClient(Collections.singletonList(new EventClient(event, null)), true);
                    appExecutors.mainThread().execute(new Runnable() {
                        @Override
                        public void run() {
                            String businessStatus = clientProcessor.calculateBusinessStatus(event);
                            presenterCallBack.onFormSaved(event.getBaseEntityId(), Task.TaskStatus.COMPLETED, businessStatus, Intervention.MOSQUITO_COLLECTION);
                        }
                    });
                } catch (Exception e) {
                    Log.e(TAG, "Error saving mosquito collection point data");
                }
            }
        };
        appExecutors.diskIO().execute(runnable);
    }


    private void saveBednetDistributionForm(JSONObject jsonForm) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    org.smartregister.domain.db.Event event = saveEvent(jsonForm, BEDNET_DISTRIBUTION_EVENT, STRUCTURE);
                    clientProcessor.processClient(Collections.singletonList(new EventClient(event, null)), true);
                    appExecutors.mainThread().execute(new Runnable() {
                        @Override
                        public void run() {
                            String businessStatus = clientProcessor.calculateBusinessStatus(event);
                            presenterCallBack.onFormSaved(event.getBaseEntityId(), Task.TaskStatus.COMPLETED, businessStatus, BEDNET_DISTRIBUTION);
                        }
                    });
                } catch (Exception e) {
                    Log.e(TAG, "Error saving bednet distribution point data");
                }
            }
        };
        appExecutors.diskIO().execute(runnable);
    }

    private void saveMemberForm(JSONObject jsonForm, String eventType, String intervention) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    org.smartregister.domain.db.Event event = saveEvent(jsonForm, eventType, FAMILY_MEMBER);
                    Client client = eventClientRepository.fetchClientByBaseEntityId(event.getBaseEntityId());
                    clientProcessor.processClient(Collections.singletonList(new EventClient(event, client)), true);
                    appExecutors.mainThread().execute(new Runnable() {
                        @Override
                        public void run() {
                            String businessStatus = clientProcessor.calculateBusinessStatus(event);
                            presenterCallBack.onFormSaved(event.getBaseEntityId(), Task.TaskStatus.COMPLETED, businessStatus, intervention);
                        }
                    });
                } catch (Exception e) {
                    Log.e(TAG, "Error saving bednet distribution point data");
                }
            }
        };
        appExecutors.diskIO().execute(runnable);
    }


}
