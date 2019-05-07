package org.smartregister.reveal.contract;

import org.json.JSONObject;
import org.smartregister.domain.Location;
import org.smartregister.reveal.model.BaseTaskDetails;
import org.smartregister.reveal.util.RevealJsonFormUtils;

/**
 * Created by samuelgithengi on 4/18/19.
 */
public interface BaseFormFragmentContract {

    interface Presenter extends UserLocationContract.UserLocationCallback, PasswordRequestCallback {

        void onStructureFound(Location structure, BaseTaskDetails details);

    }

    interface View extends UserLocationContract.UserLocationView {
        void displayToast(String format);

        RevealJsonFormUtils getJsonFormUtils();

        void startForm(JSONObject formJSON);
    }
}
