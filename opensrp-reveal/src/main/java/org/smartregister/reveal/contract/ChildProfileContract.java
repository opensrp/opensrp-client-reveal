package org.smartregister.reveal.contract;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.annotation.WorkerThread;

import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.reveal.model.Child;
import org.smartregister.util.CallableInteractor;
import org.smartregister.util.QueryComposer;
import org.smartregister.view.contract.BaseProfileContract;

public interface ChildProfileContract {

    interface View extends BaseProfileContract.View {

        void onFetchResult(Child child);

        void setLoadingState(boolean loadingState);

        Presenter getPresenter();

        void onError(Exception e);

        void startEditForm();

        void startADRForm();

        void startJsonForm(JSONObject jsonObject, String formTitle);

        void reloadFromSource();
    }

    interface Presenter extends BaseProfileContract.Presenter {

        void fetchProfileData(String baseEntityID);

        @NonNull
        CallableInteractor getInteractor();

        Presenter usingInteractor(CallableInteractor interactor);

        @Nullable
        View getView();

        @NonNull
        Model getModel();

        void startChildRegistrationForm(Context context, String baseEntityID);

        void startADRForm(Context context, String baseEntityID);

        void updateChild(JSONObject jsonObject, Context context);

        void saveADRForm(JSONObject jsonObject, Context context);
    }

    interface Model {

        @WorkerThread
        @Nullable
        Child getChild(String baseEntityID) throws QueryComposer.InvalidQueryException;

        @WorkerThread
        JSONObject getRegistrationEditForm(Context context, String baseEntityID) throws JSONException;

        @WorkerThread
        JSONObject getADRForm(Context context, String baseEntityID) throws JSONException;

    }
}
