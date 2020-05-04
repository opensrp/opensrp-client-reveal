package org.smartregister.reveal.contract;

public interface CallableInteractorCallBack<T> {

    void onResult(T t);

    void onError(Exception ex);

}
