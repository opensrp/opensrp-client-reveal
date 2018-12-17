package org.smartregister.reveal.contract;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.annotation.StringRes;
import android.support.v4.util.Pair;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.geojson.Geometry;

import org.json.JSONObject;
import org.smartregister.domain.Campaign;
import org.smartregister.domain.Task.TaskStatus;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public interface ListTaskContract {

    interface ListTaskView {

        void showProgressDialog();

        void hideProgressDialog();

        Context getContext();

        void showOperationalAreaSelector(Pair<String, ArrayList<String>> locationHierarchy);

        void setCampaign(String campaign);

        void setOperationalArea(String operationalArea);

        void setDistrict(String district);

        void setFacility(String facility);

        void setOperator();

        void showCampaignSelector(List<String> campaigns, String entireTreeString);

        void setGeoJsonSource(@NonNull FeatureCollection featureCollection, Geometry operationalAreaGeometry);

        void lockNavigationDrawerForSelection();

        void unlockNavigationDrawer();

        void displayNotification(int title, @StringRes int message);

        void displayNotification(String message);

        void openCardView(String structureId, String taskIdentifier, String businessStatus);

        void startJsonForm(JSONObject form);

        void displaySelectedFeature(Feature feature);

        void displayToast(int zoom_in_to_select);
    }

    interface PresenterCallBack {

        void onCampaignsFetched(List<Campaign> campaigns);

        void onStructuresFetched(JSONObject structuresGeoJson, Geometry operationalAreaGeometry);

        void onSprayFormSaved(@NonNull String structureId, @NonNull String taskIdentifier,
                              @NonNull TaskStatus taskStatus, @NonNull String businessStatus);

        void onStructureAdded(Feature feature);

        void onFormSaveFailure(String eventType);
    }
}
