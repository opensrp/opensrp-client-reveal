package org.smartregister.reveal.contract;

import android.content.Context;

public interface OtherFormsfragmentContract {

    interface Presenter extends BaseFormFragmentContract.Presenter{

    }

    interface View extends BaseFormFragmentContract.View {
        Context getContext();
    }
}
