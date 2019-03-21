package org.smartregister.reveal.contract;

import android.support.annotation.NonNull;
import android.support.annotation.StringRes;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.geojson.Geometry;
import com.mapbox.mapboxsdk.geometry.LatLng;

import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.domain.Task.TaskStatus;
import org.smartregister.reveal.contract.UserLocationContract.UserLocationView;
import org.smartregister.reveal.model.CardDetails;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public interface ListTaskContract {

    interface ListTaskView extends UserLocationView, BaseDrawerContract.View {

        void closeStructureCardView();

        void setGeoJsonSource(@NonNull FeatureCollection featureCollection, Geometry operationalAreaGeometry);

        void openCardView(CardDetails cardDetails);

        void startJsonForm(JSONObject form);

        void displaySelectedFeature(Feature feature, LatLng clickedPoint);

        void clearSelectedFeature();

        void displayToast(@StringRes int resourceId);
    }

    interface PresenterCallBack {

        void onStructuresFetched(JSONObject structuresGeoJson, Geometry operationalAreaGeometry);

        void onSprayFormSaved(@NonNull String structureId, @NonNull String taskIdentifier,
                              @NonNull TaskStatus taskStatus, @NonNull String businessStatus);

        void onStructureAdded(Feature feature, JSONArray featureCoordinates);

        void onFormSaveFailure(String eventType);

        void onCardDetailsFetched(CardDetails cardDetails);

        void onSprayFormDetailsFetched(CardDetails finalCardDetails);
    }

}
