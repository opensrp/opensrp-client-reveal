package org.smartregister.reveal.interactor;

import org.smartregister.reveal.contract.OtherFormsContract;
import org.smartregister.tasking.contract.BaseContract;
import org.smartregister.tasking.interactor.BaseInteractor;

public class OtherFormsInteractor extends BaseInteractor implements OtherFormsContract.Interactor {

    public OtherFormsInteractor(BaseContract.BasePresenter presenterCallBack) {
        super(presenterCallBack);
    }
}
