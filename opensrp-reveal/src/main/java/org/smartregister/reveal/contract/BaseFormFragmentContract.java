package org.smartregister.reveal.contract;

import androidx.core.util.Pair;

import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.domain.Location;
import org.smartregister.reveal.model.BaseTaskDetails;
import org.smartregister.reveal.util.RevealJsonFormUtils;

/**
 * Created by samuelgithengi on 4/18/19.
 */
public interface BaseFormFragmentContract {

    interface Presenter extends UserLocationContract.UserLocationCallback, PasswordRequestCallback {

        void onStructureFound(Location structure, BaseTaskDetails details);

        void onFetchedMembersCount(Pair<Integer, Integer> numberOfMembers, JSONObject formJSON);

        void onFetchedFamilyMembers(JSONArray familyMembers, JSONObject formJSON);

        void onFetchedSprayDetails(CommonPersonObject commonPersonObject, JSONObject formJSON);
    }

    interface View extends UserLocationContract.UserLocationView {
        void displayToast(String format);

        RevealJsonFormUtils getJsonFormUtils();

        void startForm(JSONObject formJSON);

        void displayError(int title, int message);
    }

    interface Interactor {

        void findNumberOfMembers(String structureId, JSONObject formJSON);

        void findMemberDetails(String structureId, JSONObject formJSON);

        void findSprayDetails(String interventionType, String structureId, JSONObject formJSON);
    }
}
