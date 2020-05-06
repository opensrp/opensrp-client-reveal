package org.smartregister.reveal.presenter;

import android.support.annotation.Nullable;

import org.smartregister.reveal.contract.ChildFilterFragmentContract;
import org.smartregister.util.CallableInteractor;
import org.smartregister.util.CallableInteractorCallBack;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.concurrent.Callable;

public class ChildFilterFragmentPresenter implements ChildFilterFragmentContract.Presenter {

    private WeakReference<ChildFilterFragmentContract.View> weakReference;
    private CallableInteractor interactor;
    private ChildFilterFragmentContract.Model model;

    @Override
    public void fetchUniqueGrades(String schoolID) {
        ChildFilterFragmentContract.Model myModel = getModel();
        CallableInteractor myInteractor = getInteractor();

        if (myModel != null && myInteractor != null && getView() != null) {
            Callable<List<String>> callable = () -> myModel.fetchUniqueGrades(schoolID);
            getView().setLoadingState(true);

            myInteractor.execute(callable, new CallableInteractorCallBack<List<String>>() {
                @Override
                public void onResult(List<String> results) {
                    ChildFilterFragmentContract.View view = getView();
                    if (view != null) {
                        view.onGradesFetched(results);
                        view.setLoadingState(false);
                    }
                }

                @Override
                public void onError(Exception ex) {
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
        CallableInteractor myInteractor = getInteractor();

        if (myModel != null && myInteractor != null && getView() != null) {
            Callable<List<String>> callable = () -> myModel.fetchUniqueAges(schoolID);
            getView().setLoadingState(true);

            myInteractor.execute(callable, new CallableInteractorCallBack<List<String>>() {
                @Override
                public void onResult(List<String> results) {
                    ChildFilterFragmentContract.View view = getView();
                    if (view != null) {
                        view.onAgesFetched(results);
                        view.setLoadingState(false);
                    }
                }

                @Override
                public void onError(Exception ex) {
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
    public CallableInteractor getInteractor() {
        return interactor;
    }

    @Override
    public ChildFilterFragmentContract.Presenter usingInteractor(CallableInteractor interactor) {
        this.interactor = interactor;
        return this;
    }

}
