package org.smartregister.reveal.contract;

import org.smartregister.view.contract.BaseRegisterContract;

/**
 * Created by Richard Kareko on 7/31/20.
 */

public interface EventRegisterActivityContract {

    interface View extends BaseRegisterContract.View {

        void saveJsonForm(String json);

    }

    interface Presenter extends BaseContract.BasePresenter {

        void saveJsonForm(String json);

    }

    interface Interactor extends BaseContract.BaseInteractor {

    }
}
