package org.smartregister.reveal.contract;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.annotation.WorkerThread;

import org.smartregister.util.CallableInteractor;

import java.util.HashMap;
import java.util.List;

public interface ChildFilterFragmentContract {

    interface View {

        void bindLayout();

        @NonNull
        Presenter loadPresenter();

        void reloadParameters();

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
        CallableInteractor getInteractor();

        Presenter usingInteractor(CallableInteractor interactor);
    }

    interface Model {

        @WorkerThread
        List<String> fetchUniqueGrades(String schoolID);

        @WorkerThread
        List<String> fetchUniqueAges(String schoolID);

    }
}
