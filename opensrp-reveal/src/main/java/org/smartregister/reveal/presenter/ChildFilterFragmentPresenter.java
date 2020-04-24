package org.smartregister.reveal.presenter;

import android.support.annotation.Nullable;

import org.smartregister.reveal.contract.ChildFilterFragmentContract;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.concurrent.Callable;

public class ChildFilterFragmentPresenter implements ChildFilterFragmentContract.Presenter {

    private WeakReference<ChildFilterFragmentContract.View> weakReference;
    private ChildFilterFragmentContract.Interactor<List<String>> interactor;
    private ChildFilterFragmentContract.Model model;

    @Override
    public void fetchUniqueGrades(String schoolID) {
        ChildFilterFragmentContract.Model myModel = getModel();
        ChildFilterFragmentContract.Interactor<List<String>> myInteractor = getInteractor();

        if (myModel != null && myInteractor != null && getView() != null) {
            Callable<List<String>> callable = () -> myModel.fetchUniqueGrades(schoolID);
            getView().setLoadingState(true);

            myInteractor.runRequest(callable, new ChildFilterFragmentContract.InteractorCallBack<List<String>>() {
                @Override
                public void onFetchResults(List<String> results) {
                    ChildFilterFragmentContract.View view = getView();
                    if (view != null) {
                        view.onGradesFetched(results);
                        view.setLoadingState(false);
                    }
                }

                @Override
                public void onFetchError(Exception ex) {
                    ChildFilterFragmentContract.View view = getView();
                    if (view != null) {
                        view.onError(ex);
                        view.setLoadingState(false);
                    }
                }
            });
        }
    }

    @Override
    public void fetchUniqueAges(String schoolID) {
        ChildFilterFragmentContract.Model myModel = getModel();
        ChildFilterFragmentContract.Interactor<List<String>> myInteractor = getInteractor();

        if (myModel != null && myInteractor != null && getView() != null) {
            Callable<List<String>> callable = () -> myModel.fetchUniqueAges(schoolID);
            getView().setLoadingState(true);

            myInteractor.runRequest(callable, new ChildFilterFragmentContract.InteractorCallBack<List<String>>() {
                @Override
                public void onFetchResults(List<String> results) {
                    ChildFilterFragmentContract.View view = getView();
                    if (view != null) {
                        view.onAgesFetched(results);
                        view.setLoadingState(false);
                    }
                }

                @Override
                public void onFetchError(Exception ex) {
                    ChildFilterFragmentContract.View view = getView();
                    if (view != null) {
                        view.onError(ex);
                        view.setLoadingState(false);
                    }
                }
            });
        }
    }

    @Override
    public ChildFilterFragmentContract.Presenter usingView(ChildFilterFragmentContract.View view) {
        this.weakReference = new WeakReference<>(view);
        return this;
    }

    @Nullable
    @Override
    public ChildFilterFragmentContract.View getView() {
        if (weakReference != null)
            return weakReference.get();

        return null;
    }

    @Nullable
    @Override
    public ChildFilterFragmentContract.Model getModel() {
        return this.model;
    }

    @Override
    public ChildFilterFragmentContract.Presenter usingModel(ChildFilterFragmentContract.Model model) {
        this.model = model;
        return this;
    }

    @Nullable
    @Override
    public ChildFilterFragmentContract.Interactor<List<String>> getInteractor() {
        return interactor;
    }

    @Override
    public ChildFilterFragmentContract.Presenter usingInteractor(ChildFilterFragmentContract.Interactor<List<String>> interactor) {
        this.interactor = interactor;
        return this;
    }

}
