package org.smartregister.reveal.contract;

import android.content.Context;

import org.smartregister.tasking.contract.BaseFormFragmentContract;

public interface OtherFormsfragmentContract {

    interface Presenter extends BaseFormFragmentContract.Presenter{

    }

    interface View extends BaseFormFragmentContract.View {
        Context getContext();
    }
}
