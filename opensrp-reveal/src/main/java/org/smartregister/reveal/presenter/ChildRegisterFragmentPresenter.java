package org.smartregister.reveal.presenter;

import android.content.Context;
import android.support.annotation.Nullable;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.vijay.jsonwizard.constants.JsonFormConstants;

import org.joda.time.DateTime;
import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.CoreLibrary;
import org.smartregister.clientandeventmodel.Client;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.domain.Location;
import org.smartregister.domain.LocationProperty;
import org.smartregister.domain.db.EventClient;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.ChildRegisterFragmentContract;
import org.smartregister.reveal.model.Child;
import org.smartregister.reveal.model.ChildModel;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.JsonClientProcessingUtils;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.reveal.util.Utils;
import org.smartregister.sync.helper.ECSyncHelper;
import org.smartregister.util.AppExecutors;
import org.smartregister.util.CallableInteractor;
import org.smartregister.util.CallableInteractorCallBack;
import org.smartregister.util.DateTimeTypeConverter;
import org.smartregister.util.GenericInteractor;
import org.smartregister.util.JsonFormUtils;
import org.smartregister.util.PropertiesConverter;
import org.smartregister.view.ListContract;
import org.smartregister.view.presenter.ListPresenter;

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.Callable;

import static org.smartregister.reveal.application.RevealApplication.getInstance;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CHILD_TABLE;
import static org.smartregister.reveal.util.Constants.METADATA;
import static org.smartregister.reveal.util.Constants.REGISTER_CHILD_EVENT;
import static org.smartregister.util.JsonFormUtils.getJSONObject;

public class ChildRegisterFragmentPresenter extends ListPresenter<Child> implements ChildRegisterFragmentContract.Presenter {

    public static final Gson gson = new GsonBuilder().setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
            .registerTypeAdapter(DateTime.class, new DateTimeTypeConverter())
            .registerTypeAdapter(LocationProperty.class, new PropertiesConverter()).create();

    private CallableInteractor callableInteractor;

    @Override
    public void search(String schoolID, @Nullable HashMap<String, List<String>> sortAndFilter, @Nullable String searchText) {
        ChildModel model = getModel();
        this.fetchList(() -> model.searchAndFilter(schoolID, sortAndFilter, searchText), AppExecutors.Request.DISK_THREAD);
    }

    @Override
    public void startMDAForm(Context context, String baseEntityID) {
        CallableInteractor myInteractor = getCallableInteractor();
        ChildModel model = getModel();
        if (model != null) {
            Callable<JSONObject> callable = () -> model.getMDAForm(context, baseEntityID);
            myInteractor.execute(callable, new CallableInteractorCallBack<JSONObject>() {
                @Override
                public void onResult(JSONObject jsonObject) {
                    ChildRegisterFragmentContract.View view = getView();
                    if (view != null) {
                        if (jsonObject != null) {
                            view.startJsonForm(jsonObject, context.getString(R.string.record_dose));
                        } else {
                            view.onFetchError(new IllegalArgumentException("Form not found"));
                        }
                        view.setLoadingState(false);
                    }
                }

                @Override
                public void onError(Exception ex) {
                    ListContract.View<Child> view = getView();
                    if (view != null) {
                        view.onFetchError(ex);
                        view.setLoadingState(false);
                    }
                }
            });
        }
    }

    @Override
    public void startChildRegistrationForm(Context context) {
        CallableInteractor myInteractor = getCallableInteractor();
        ChildModel model = getModel();
        if (model != null) {
            Callable<JSONObject> callable = () -> model.getRegistrationForm(context);
            myInteractor.execute(callable, new CallableInteractorCallBack<JSONObject>() {
                @Override
                public void onResult(JSONObject jsonObject) {
                    ChildRegisterFragmentContract.View view = getView();
                    if (view != null) {
                        if (jsonObject != null) {
                            view.startJsonForm(jsonObject, context.getString(R.string.add_student));
                        } else {
                            view.onFetchError(new IllegalArgumentException("Form not found"));
                        }
                        view.setLoadingState(false);
                    }
                }

                @Override
                public void onError(Exception ex) {
                    ListContract.View<Child> view = getView();
                    if (view != null) {
                        view.onFetchError(ex);
                        view.setLoadingState(false);
                    }
                }
            });
        }
    }

    @Override
    public CallableInteractor getCallableInteractor() {
        if (callableInteractor == null)
            callableInteractor = new GenericInteractor();

        return callableInteractor;
    }

    @Override
    public void saveChild(String jsonString, Context context) {
        CallableInteractor myInteractor = getCallableInteractor();

        Callable<Boolean> callable = () -> {

            // populate metadata
            AllSharedPreferences allSharedPreferences = CoreLibrary.getInstance().context().allSharedPreferences();
            Location operationalArea = Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());

            RevealJsonFormUtils revealJsonFormUtils = new RevealJsonFormUtils();

            String entityId = UUID.randomUUID().toString();
            revealJsonFormUtils.populateFormDetails(jsonString, entityId, operationalArea.getId(), null, null, null, operationalArea.getId(), operationalArea.getServerVersion().intValue());

            JSONObject jsonForm = new JSONObject(jsonString);
            JSONArray fields = jsonForm.getJSONObject(JsonFormConstants.STEP1).getJSONArray(JsonFormConstants.FIELDS);

            Client baseClient = org.smartregister.util.JsonFormUtils.createBaseClient(fields, JsonClientProcessingUtils.formTag(allSharedPreferences), entityId);
            Event baseEvent = org.smartregister.util.JsonFormUtils.createEvent(fields, getJSONObject(jsonForm, METADATA), JsonClientProcessingUtils.formTag(allSharedPreferences), entityId, REGISTER_CHILD_EVENT, CHILD_TABLE);
            JsonClientProcessingUtils.tagSyncMetadata(allSharedPreferences, baseEvent);

            // save the client
            JSONObject clientJson = new JSONObject(org.smartregister.family.util.JsonFormUtils.gson.toJson(baseClient));
            getSyncHelper().addClient(baseClient.getBaseEntityId(), clientJson);

            // save the event
            JSONObject eventJson = new JSONObject(org.smartregister.family.util.JsonFormUtils.gson.toJson(baseEvent));
            getSyncHelper().addEvent(baseEvent.getBaseEntityId(), eventJson);

            // client process

            org.smartregister.domain.db.Event domainEvent = org.smartregister.family.util.JsonFormUtils.gson.fromJson(eventJson.toString(), org.smartregister.domain.db.Event.class);
            org.smartregister.domain.db.Client domainClient = org.smartregister.family.util.JsonFormUtils.gson.fromJson(clientJson.toString(), org.smartregister.domain.db.Client.class);
            EventClient eventClient = new EventClient(domainEvent, domainClient);

            RevealClientProcessor.getInstance(getInstance().getApplicationContext()).processClient(Collections.singletonList(eventClient), true);

            long lastSyncTimeStamp = allSharedPreferences.fetchLastUpdatedAtDate(0);
            Date lastSyncDate = new Date(lastSyncTimeStamp);
            allSharedPreferences.saveLastUpdatedAtDate(lastSyncDate.getTime());


            // create a task
            TaskUtils taskUtils = TaskUtils.getInstance();
            taskUtils.generateDrugAdministrationTask(context, entityId);

            // extract and close ids
            JSONObject field = JsonFormUtils.getFieldJSONObject(JsonFormUtils.fields(jsonForm), Constants.DatabaseKeys.UNIQUE_ID);
            if (field != null) {
                String uniqueID = field.getString(JsonFormConstants.VALUE);
                CoreLibrary.getInstance().context().getUniqueIdRepository().close(uniqueID);
            }
            return true;
        };

        myInteractor.execute(callable, new CallableInteractorCallBack<Boolean>() {
            @Override
            public void onResult(Boolean aBoolean) {
                ChildRegisterFragmentContract.View view = getView();
                if (view != null) {
                    if (aBoolean) {
                        view.reloadFromSource();
                    } else {
                        view.onFetchError(new Exception("An error while saving"));
                    }
                    view.setLoadingState(false);
                }
            }

            @Override
            public void onError(Exception ex) {
                ChildRegisterFragmentContract.View view = getView();
                if (view == null) return;
                view.onFetchError(ex);
                view.setLoadingState(false);
            }
        });

    }

    public ECSyncHelper getSyncHelper() {
        return ECSyncHelper.getInstance(RevealApplication.getInstance().getContext().applicationContext());
    }

    @Override
    public ChildRegisterFragmentContract.View getView() {
        return (ChildRegisterFragmentContract.View) super.getView();
    }

}
