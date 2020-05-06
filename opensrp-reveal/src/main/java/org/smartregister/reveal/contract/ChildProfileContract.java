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

        void startJsonForm(JSONObject form);
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
    }

    interface Model {

        @WorkerThread
        @Nullable
        Child getChild(String baseEntityID) throws QueryComposer.InvalidQueryException;

        @WorkerThread
        JSONObject getRegistrationEditForm(Context context, String baseEntityID) throws JSONException;

    }
}
