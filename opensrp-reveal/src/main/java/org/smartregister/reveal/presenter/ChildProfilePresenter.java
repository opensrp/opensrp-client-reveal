package org.smartregister.reveal.presenter;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;

import org.json.JSONObject;
import org.smartregister.domain.Location;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.ChildProfileContract;
import org.smartregister.reveal.model.Child;
import org.smartregister.reveal.model.ChildProfileModel;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.NativeFormProcessor;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;
import org.smartregister.util.CallableInteractor;
import org.smartregister.util.CallableInteractorCallBack;

import java.lang.ref.WeakReference;
import java.util.concurrent.Callable;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.CHILD_TABLE;
import static org.smartregister.reveal.util.Constants.EventType.UPDATE_CHILD_REGISTRATION;

public class ChildProfilePresenter implements ChildProfileContract.Presenter {

    private CallableInteractor callableInteractor;
    private WeakReference<ChildProfileContract.View> viewWeakReference;
    private ChildProfileContract.Model model;

    public ChildProfilePresenter(ChildProfileContract.View view) {
        this.viewWeakReference = new WeakReference<>(view);
    }

    @Override
    public void fetchProfileData(String baseEntityID) {
        CallableInteractor myInteractor = getInteractor();
        ChildProfileContract.Model myModel = getModel();

        if (getView() != null) {
            Callable<Child> callable = () -> myModel.getChild(baseEntityID);
            getView().setLoadingState(true);

            myInteractor.execute(callable, new CallableInteractorCallBack<Child>() {
                @Override
                public void onResult(Child result) {
                    ChildProfileContract.View view = getView();
                    if (view != null) {
                        if (result != null) {
                            view.onFetchResult(result);
                        } else {
                            view.onError(new IllegalStateException("Child not found"));
                        }
                        view.setLoadingState(false);
                    }
                }

                @Override
                public void onError(Exception ex) {
                    ChildProfileContract.View view = getView();
                    if (view != null) {
                        view.onError(ex);
                        view.setLoadingState(false);
                    }
                }
            });
        }
    }

    @NonNull
    @Override
    public CallableInteractor getInteractor() {
        if (callableInteractor == null)
            throw new IllegalStateException("Interacator is not set");

        return callableInteractor;
    }

    @Override
    public ChildProfileContract.Presenter usingInteractor(CallableInteractor interactor) {
        this.callableInteractor = interactor;
        return this;
    }

    @Nullable
    @Override
    public ChildProfileContract.View getView() {
        if (viewWeakReference != null)
            return viewWeakReference.get();

        return null;
    }

    @NonNull
    @Override
    public ChildProfileContract.Model getModel() {
        if (model == null)
            model = new ChildProfileModel();

        return model;
    }

    @Override
    public void startChildRegistrationForm(Context context, String baseEntityID) {
        CallableInteractor myInteractor = getInteractor();
        ChildProfileContract.Model model = getModel();
        Callable<JSONObject> callable = () -> model.getRegistrationEditForm(context, baseEntityID);
        myInteractor.execute(callable, new CallableInteractorCallBack<JSONObject>() {
            @Override
            public void onResult(JSONObject jsonObject) {
                ChildProfileContract.View view = getView();
                if (view != null) {
                    if (jsonObject != null) {
                        view.startJsonForm(jsonObject, context.getString(R.string.edit_student));
                    } else {
                        view.onError(new IllegalArgumentException("Form not found"));
                    }
                    view.setLoadingState(false);
                }
            }

            @Override
            public void onError(Exception ex) {
                ChildProfileContract.View view = getView();
                if (view != null) {
                    view.onError(ex);
                    view.setLoadingState(false);
                }
            }
        });
    }

    @Override
    public void startADRForm(Context context, String baseEntityID) {
        CallableInteractor myInteractor = getInteractor();
        ChildProfileContract.Model model = getModel();
        Callable<JSONObject> callable = () -> model.getADRForm(context, baseEntityID);
        myInteractor.execute(callable, new CallableInteractorCallBack<JSONObject>() {
            @Override
            public void onResult(JSONObject jsonObject) {
                ChildProfileContract.View view = getView();
                if (view != null) {
                    if (jsonObject != null) {
                        view.startJsonForm(jsonObject, context.getString(R.string.record_adr));
                    } else {
                        view.onError(new IllegalArgumentException("Form not found"));
                    }
                    view.setLoadingState(false);
                }
            }

            @Override
            public void onError(Exception ex) {
                ChildProfileContract.View view = getView();
                if (view != null) {
                    view.onError(ex);
                    view.setLoadingState(false);
                }
            }
        });
    }

    @Override
    public void updateChild(JSONObject jsonObject, Context context) {
        CallableInteractor myInteractor = getInteractor();

        Callable<Boolean> callable = () -> {

            Location operationalArea = Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
            String entityId = jsonObject.getString(Constants.Properties.BASE_ENTITY_ID);
            new NativeFormProcessor(jsonObject)

                    // update metadata
                    .withBindType(CHILD_TABLE)
                    .withEncounterType(UPDATE_CHILD_REGISTRATION)
                    .withEntityId(entityId)
                    .tagLocationData(operationalArea)
                    .tagEventMetadata()

                    // create and save client
                    .hasClient(true)
                    .mergeAndSaveClient()

                    // create and save event to db
                    .saveEvent()

                    // execute client processing
                    .clientProcessForm();

            return true;
        };

        myInteractor.execute(callable, new CallableInteractorCallBack<Boolean>() {
            @Override
            public void onResult(Boolean aBoolean) {
                ChildProfileContract.View view = getView();
                if (view != null) {
                    if (aBoolean) {
                        view.reloadFromSource();
                    } else {
                        view.onError(new Exception("An error while saving"));
                    }
                    view.setLoadingState(false);
                }
            }

            @Override
            public void onError(Exception ex) {
                ChildProfileContract.View view = getView();
                if (view == null) return;
                view.onError(ex);
                view.setLoadingState(false);
            }
        });
    }

    @Override
    public void saveADRForm(JSONObject jsonObject, Context context) {
        CallableInteractor myInteractor = getInteractor();

        Callable<Boolean> callable = () -> {

            String entityId = jsonObject.getString(Constants.Properties.BASE_ENTITY_ID);

            // save event details
            Location operationalArea = Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());

            // update metadata
            new NativeFormProcessor(jsonObject)
                    .withBindType(Constants.EventType.MDA_ADVERSE_DRUG_REACTION)
                    .withEncounterType(Constants.EventType.MDA_ADVERSE_DRUG_REACTION)
                    .withEntityId(entityId)
                    .tagLocationData(operationalArea)
                    .tagEventMetadata()

                    // save and client
                    .saveEvent()
                    .clientProcessForm();

            return true;
        };

        myInteractor.execute(callable, new CallableInteractorCallBack<Boolean>() {
            @Override
            public void onResult(Boolean aBoolean) {
                ChildProfileContract.View view = getView();
                if (view != null) {
                    if (aBoolean) {
                        view.reloadFromSource();
                    } else {
                        view.onError(new Exception("An error while saving"));
                    }
                    view.setLoadingState(false);
                }
            }

            @Override
            public void onError(Exception ex) {
                ChildProfileContract.View view = getView();
                if (view == null) return;
                view.onError(ex);
                view.setLoadingState(false);
            }
        });
    }

    @Override
    public void onDestroy(boolean b) {
        Timber.v("onDestroy");
    }
}
