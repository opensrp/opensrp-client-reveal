package org.smartregister.reveal.interactor;

import android.support.annotation.VisibleForTesting;

import org.smartregister.reveal.contract.ChildFilterFragmentContract;
import org.smartregister.util.AppExecutors;

import java.util.concurrent.Callable;

import timber.log.Timber;

public class ChildFilterFragmentInteractor<T> implements ChildFilterFragmentContract.Interactor<T> {

    protected AppExecutors appExecutors;

    public ChildFilterFragmentInteractor() {
        appExecutors = new AppExecutors();
    }

    @VisibleForTesting
    ChildFilterFragmentInteractor(AppExecutors appExecutors) {
        this.appExecutors = appExecutors;
    }

    @Override
    public void runRequest(Callable<T> callable, ChildFilterFragmentContract.InteractorCallBack<T> callBack) {
        Runnable runnable = () -> {
            try {
                T result = callable.call();
                appExecutors.mainThread().execute(() -> callBack.onFetchResults(result));
            } catch (Exception e) {
                Timber.e(e);
                appExecutors.mainThread().execute(() -> callBack.onFetchError(e));
            }
        };
        appExecutors.execute(runnable, AppExecutors.Request.DISK_THREAD);
    }
}
