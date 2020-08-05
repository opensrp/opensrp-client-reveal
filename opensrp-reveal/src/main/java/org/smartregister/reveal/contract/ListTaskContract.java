package org.smartregister.reveal.contract;

import android.content.Context;
import androidx.annotation.NonNull;
import androidx.annotation.StringRes;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.vijay.jsonwizard.domain.Form;

import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Task;
import org.smartregister.reveal.contract.UserLocationContract.UserLocationView;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.util.CallableInteractor;

import java.util.List;
import java.util.Map;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public interface ListTaskContract {

    interface ListTaskView extends UserLocationView, BaseDrawerContract.DrawerActivity {

        void showProgressDialog(@StringRes int title, @StringRes int message);

        void hideProgressDialog();

        Context getContext();

        void closeCardView(int id);

        void closeAllCardViews();

        void openFilterTaskActivity(TaskFilterParams filterParams);

        void openTaskRegister(TaskFilterParams filterParams);

        void openStructureProfile(CommonPersonObjectClient family);

        void registerFamily();

        void setGeoJsonSource(@NonNull FeatureCollection featureCollection, Feature operationalArea, boolean changeMapPosition);

        void displayNotification(int title, @StringRes int message, Object... formatArgs);

        void displayNotification(String message);

        void displayNotification(String title, String message);

        void openCardView(CardDetails cardDetails);

        void startJsonForm(JSONObject form);

        void displaySelectedFeature(Feature feature, LatLng clickedPoint);

        void displaySelectedFeature(Feature feature, LatLng clickedPoint, double zoomlevel);

        void clearSelectedFeature();

        void displayToast(@StringRes int resourceId);

        RevealJsonFormUtils getJsonFormUtils();

        void focusOnUserLocation(boolean focusOnUserLocation);

        boolean isMyLocationComponentActive();

        void displayMarkStructureInactiveDialog();

        void setNumberOfFilters(int numberOfFilters);

        void setSearchPhrase(String searchPhrase);

        void startEligibilityForm();

        /**
         * Strart structure issue wizard
         * @param structureId
         */
        void issueStructureQRCode(String structureId, String pendingTask);

        void setLoadingState(boolean state);

        void onError(Exception e);

        void onEligibilityStatusConfirmed(String status);

        void startJSONForm(JSONObject jsonObject, Form form);

        void toggleProgressBarView(boolean syncing);

        void onReportCountReloaded(Map<String, Integer> reportCounts);
    }

    interface Presenter extends BaseContract.BasePresenter {

        void onStructuresFetched(JSONObject structuresGeoJson, Feature operationalArea, List<TaskDetails> taskDetailsList);

        void onDrawerClosed();

        void resetFeatureTasks(String structureId, Task task);

        void onStructureAdded(Feature feature, JSONArray featureCoordinates, double zoomlevel);

        void onFormSaveFailure(String eventType);

        void onCardDetailsFetched(CardDetails cardDetails);

        void onInterventionFormDetailsFetched(CardDetails finalCardDetails);

        void onInterventionTaskInfoReset(boolean success);

        Feature getSelectedFeature();

        @StringRes
        int getInterventionLabel();

        void onMarkStructureInactiveConfirmed();

        void onStructureMarkedInactive();

        void onMarkStructureIneligibleConfirmed();

        void onStructureMarkedIneligible();

        void validateUserLocation();

        void onFilterTasksClicked();

        void onOpenTaskRegisterClicked();

        void setTaskFilterParams(TaskFilterParams filterParams);

        void startFamilyProfileByQR(String qrCode);

        void saveEligibilityForm(JSONObject jsonObject, Feature feature);

        void assignQRCodeToStructure(Context context, String structureId, String qrcode, Runnable runnable);

        ListTaskView getView();

        @NonNull
        CallableInteractor getInteractor();

        void startFamilyRegistrationForm(Context context);

        void saveFamilyRegistration(JSONObject jsonObject, Context context);

        void onFociBoundaryLongClicked();

        void onMarkStructureInEligible(Feature feature);

        void clearSelectedFeature();

        void fetchReportStats();
    }
}
