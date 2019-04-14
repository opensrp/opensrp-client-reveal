package org.smartregister.reveal.contract;

import android.content.Context;
import android.location.LocationListener;

import org.json.JSONObject;
import org.smartregister.domain.Location;
import org.smartregister.reveal.adapter.StructureTaskAdapter;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.RevealJsonFormUtils;

import java.util.List;

import io.ona.kujaku.listeners.BaseLocationListener;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public interface StructureTasksContract {

    interface Presenter {

        void findTasks(String structureId);

        void onTasksFound(List<StructureTaskDetails> taskDetailsList);

        void onTaskSelected(StructureTaskDetails details);

        void onStructureFound(Location structure, StructureTaskDetails details);
    }

    interface Interactor {

        void findTasks(String structureId, String currentCampaignId);

        void getStructure(StructureTaskDetails details);
    }

    interface View extends UserLocationContract.UserLocationView {

        StructureTaskAdapter getAdapter();

        void setStructure(String structureId);

        void displayToast(String message);

        void showProgressDialog(int title, int message);

        void hideProgressDialog();

        android.location.Location getUserCurrentLocation();

        Context getContext();

        RevealJsonFormUtils getJsonFormUtils();

        void startForm(JSONObject formJSON);
    }
}
