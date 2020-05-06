package org.smartregister.reveal.presenter;

import android.content.Context;
import android.support.annotation.Nullable;

import org.json.JSONObject;
import org.smartregister.reveal.contract.ChildRegisterFragmentContract;
import org.smartregister.reveal.model.Child;
import org.smartregister.reveal.model.ChildModel;
import org.smartregister.util.AppExecutors;
import org.smartregister.util.CallableInteractor;
import org.smartregister.util.CallableInteractorCallBack;
import org.smartregister.util.GenericInteractor;
import org.smartregister.view.ListContract;
import org.smartregister.view.presenter.ListPresenter;

import java.util.HashMap;
import java.util.List;
import java.util.concurrent.Callable;

public class ChildRegisterFragmentPresenter extends ListPresenter<Child> implements ChildRegisterFragmentContract.Presenter {

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
                            view.startJsonForm(jsonObject);
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
                            view.startJsonForm(jsonObject);
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
    public ChildRegisterFragmentContract.View getView() {
        return (ChildRegisterFragmentContract.View) super.getView();
    }

}
