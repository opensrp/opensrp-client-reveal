package org.smartregister.reveal.contract;

import java.util.concurrent.Callable;

public interface CallableInteractor {

    <T> void execute(Callable<T> callable, CallableInteractorCallBack<T> callBack);

}
