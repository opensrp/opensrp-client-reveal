package org.smartregister.reveal.presenter;

import org.smartregister.reveal.contract.OtherFormsfragmentContract;
import org.smartregister.tasking.presenter.BaseFormFragmentPresenter;

public class OtherFormsFragmentPresenter extends BaseFormFragmentPresenter {

    public OtherFormsFragmentPresenter(OtherFormsfragmentContract.View view) {
        super(view, view.getContext());
    }
}
