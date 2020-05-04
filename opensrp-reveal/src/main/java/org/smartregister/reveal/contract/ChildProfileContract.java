package org.smartregister.reveal.contract;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.annotation.WorkerThread;

import org.smartregister.reveal.model.Child;
import org.smartregister.util.QueryComposer;
import org.smartregister.view.contract.BaseProfileContract;

public interface ChildProfileContract {

    interface View extends BaseProfileContract.View {

        void onFetchResult(Child child);

        void setLoadingState(boolean loadingState);

        Presenter getPresenter();

        void onError(Exception e);

        void startEditForm();
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
    }

    interface Model {

        @WorkerThread
        @Nullable
        Child getChild(String baseEntityID) throws QueryComposer.InvalidQueryException;

    }
}
