package org.smartregister.reveal.contract;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.annotation.WorkerThread;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

public interface ChildFilterFragmentContract {

    interface View {

        void bindLayout();

        @NonNull
        Presenter loadPresenter();

        void reloadParameters();

        void resetFilters(Map<String, List<String>> filters);

        HashMap<String, List<String>> getFilterValues();

        void clearFilters();

        void onGradesFetched(List<String> grades);

        void onAgesFetched(List<String> ages);

        void onError(Exception e);

        void setLoadingState(boolean loadingState);

        void executeFilter();
    }

    interface Presenter {

        void fetchUniqueGrades(String schoolID);

        void fetchUniqueAges(String schoolID);

        Presenter usingView(ChildFilterFragmentContract.View view);

        @Nullable
        View getView();

        @Nullable
        Model getModel();

        Presenter usingModel(Model model);

        @Nullable
        Interactor<List<String>> getInteractor();

        Presenter usingInteractor(Interactor<List<String>> interactor);
    }

    interface Model {

        @WorkerThread
        List<String> fetchUniqueGrades(String schoolID);

        @WorkerThread
        List<String> fetchUniqueAges(String schoolID);

    }

    interface Interactor<T> {

        void runRequest(Callable<T> callable, InteractorCallBack<T> callBack);

    }

    interface InteractorCallBack<T> {

        void onFetchResults(T results);

        void onFetchError(Exception ex);

    }
}
