package org.smartregister.reveal.contract;

import org.smartregister.view.contract.BaseRegisterFragmentContract;

/**
 * Created by samuelgithengi on 3/18/19.
 */
public interface TaskRegisterFragmentContract {

    interface Presenter extends BaseRegisterFragmentContract.Presenter {
        String countSelect();

        String mainSelect();
    }
}
