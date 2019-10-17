package org.smartregister.reveal.interactor;

import org.smartregister.reveal.contract.BaseContract;
import org.smartregister.reveal.contract.OtherFormsContract;

public class OtherFormsInteractor extends BaseInteractor implements OtherFormsContract.Interactor {

    public OtherFormsInteractor(BaseContract.BasePresenter presenterCallBack) {
        super(presenterCallBack);
    }
}
