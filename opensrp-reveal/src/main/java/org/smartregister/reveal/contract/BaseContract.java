package org.smartregister.reveal.contract;

import android.support.annotation.NonNull;

import com.mapbox.geojson.Feature;

import org.json.JSONArray;
import org.smartregister.domain.Task;
import org.smartregister.domain.Task.TaskStatus;

/**
 * Created by samuelgithengi on 3/25/19.
 */
public class BaseContract {

    public interface BasePresenter {

        void onFormSaved(@NonNull String structureId,
                         @NonNull Task.TaskStatus taskStatus, @NonNull String businessStatus, String interventionType);

        void onStructureAdded(Feature feature, JSONArray featureCoordinates);

        void onFormSaveFailure(String eventType);
    }
}
