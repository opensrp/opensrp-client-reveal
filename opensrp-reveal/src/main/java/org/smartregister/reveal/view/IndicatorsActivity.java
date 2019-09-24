package org.smartregister.reveal.view;

import android.app.Activity;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v4.util.Pair;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.mapbox.geojson.Feature;
import com.mapbox.mapboxsdk.geometry.LatLng;

import org.joda.time.DateTime;
import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Location;
import org.smartregister.domain.LocationProperty;
import org.smartregister.domain.Task;
import org.smartregister.reporting.view.TableView;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.TaskRegisterFragmentContract;
import org.smartregister.reveal.interactor.TaskRegisterFragmentInteractor;
import org.smartregister.reveal.model.BaseTaskDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.presenter.ValidateUserLocationPresenter;
import org.smartregister.reveal.repository.RevealMappingHelper;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.util.DateTimeTypeConverter;
import org.smartregister.util.PropertiesConverter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by ndegwamartin on 2019-09-23.
 */
public class IndicatorsActivity extends Activity implements TaskRegisterFragmentContract.Presenter {

    public static final String TAG = IndicatorsActivity.class.getCanonicalName();

    private TaskRegisterFragmentInteractor taskRegisterFragmentInteractor;

    private TableView tableView;

    private android.location.Location currentLocation;

    public static final Gson gson = new GsonBuilder().setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
            .registerTypeAdapter(DateTime.class, new DateTimeTypeConverter())
            .registerTypeAdapter(LocationProperty.class, new PropertiesConverter()).create();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_indicators);

        tableView = this.findViewById(R.id.tableView);

        if (this.getIntent().getExtras() != null) {
            currentLocation = getIntent().getExtras().getParcelable(Constants.TaskRegister.LAST_USER_LOCATION);
        }

        this.taskRegisterFragmentInteractor = new TaskRegisterFragmentInteractor(this);
    }


    @Override
    public void onTasksFound(List<TaskDetails> tasks, int structuresWithinBuffer) {


        Map<String, Integer> map = new HashMap<>();

        List<String> sprayIndicator = new ArrayList<>();

        for (int i = 0; i < tasks.size(); i++) {

            String key = tasks.get(i).getSprayStatus() == null ? "NULL" : tasks.get(i).getSprayStatus();

            int newValue = map.get(key) != null ? map.get(key) + 1 : 1;

            map.put(key, newValue);

        }


        int sprayed = map.get(org.smartregister.reveal.util.Constants.BusinessStatus.SPRAYED);
        int notSprayed = map.get(org.smartregister.reveal.util.Constants.BusinessStatus.NOT_SPRAYED);
        int totalStructures = tasks.size();
        int progress = totalStructures > 0 ? Math.round(sprayed * 100 / totalStructures) : 0;
        int notVisited = map.get("NULL") != null ? map.get("NULL") : 0;

        sprayIndicator.add("Spray Coverage");
        sprayIndicator.add(progress + "%");

        int totalFound = (sprayed + notSprayed);

        sprayIndicator.add("Remaining 90%");
        sprayIndicator.add(String.valueOf(Math.round(totalStructures * 0.9) - sprayed));


        sprayIndicator.add("Total Structures");
        sprayIndicator.add(String.valueOf(totalStructures));


        sprayIndicator.add("Not Visited");
        sprayIndicator.add(String.valueOf(notVisited));


        sprayIndicator.add("Visited/Found");
        sprayIndicator.add(String.valueOf(totalFound));

        sprayIndicator.add("Structures Sprayed");
        sprayIndicator.add(String.valueOf(sprayed));


        sprayIndicator.add("Structures Not Sprayed");
        sprayIndicator.add(String.valueOf(notSprayed));


        tableView.setTableData(Arrays.asList(new String[]{"Indicator", "Value"}), sprayIndicator);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();

    }


    @Override
    public void onResume() {
        super.onResume();

        fetchTaskDetails();

    }

    @Override
    public void onDrawerClosed() {

    }

    @Override
    public void onTaskSelected(TaskDetails details, boolean isActionClicked) {

    }

    @Override
    public int getInterventionLabel() {
        return 0;
    }

    @Override
    public void onIndexCaseFound(JSONObject indexCase, boolean isLinkedToJurisdiction) {

    }

    @Override
    public void onFormSaved(@NonNull String structureId, String taskID, @NonNull Task.TaskStatus taskStatus, @NonNull String businessStatus, String interventionType) {

    }

    @Override
    public void onStructureAdded(Feature feature, JSONArray featureCoordinates, double zoomlevel) {

    }

    @Override
    public void onFormSaveFailure(String eventType) {

    }

    @Override
    public void onFamilyFound(CommonPersonObjectClient finalFamily) {

    }

    @Override
    public void onStructureFound(Location structure, BaseTaskDetails details) {

    }

    @Override
    public void onFetchedMembersCount(int numberOfMembers, JSONObject formJSON) {

    }

    @Override
    public void onFetchedFamilyMembers(JSONArray familyMembers, JSONObject formJSON) {

    }

    @Override
    public void onFetchedSprayDetails(CommonPersonObject commonPersonObject, JSONObject formJSON) {

    }

    @Override
    public void onPasswordVerified() {

    }

    @Override
    public void onLocationValidated() {

    }

    @Override
    public LatLng getTargetCoordinates() {
        return null;
    }

    @Override
    public void requestUserPassword() {

    }

    @Override
    public ValidateUserLocationPresenter getLocationPresenter() {
        return null;
    }

    @Override
    public void processViewConfigurations() {

    }

    @Override
    public void initializeQueries(String s) {

    }

    @Override
    public void startSync() {

    }

    @Override
    public void searchGlobally(String s) {

    }

    private android.location.Location getOperationalAreaCenter() {
        org.smartregister.domain.Location operationalAreaLocation = org.smartregister.reveal.util.Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        if (operationalAreaLocation == null)
            return null;
        return new RevealMappingHelper().getCenter(gson.toJson(operationalAreaLocation.getGeometry()));
    }

    private Pair<String, String[]> getMainCondition() {
        org.smartregister.domain.Location operationalArea = org.smartregister.reveal.util.Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        String whereClause = String.format("%s.%s = ? AND %s.%s = ? AND %s.%s != ?",
                org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_TABLE, org.smartregister.reveal.util.Constants.DatabaseKeys.GROUPID, org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_TABLE, org.smartregister.reveal.util.Constants.DatabaseKeys.PLAN_ID,
                org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_TABLE, org.smartregister.reveal.util.Constants.DatabaseKeys.STATUS);
        return new Pair<>(whereClause, new String[]{operationalArea == null ?
                null : operationalArea.getId(), PreferencesUtil.getInstance().getCurrentPlanId(), Task.TaskStatus.CANCELLED.name()});
    }

    private void fetchTaskDetails() {


        taskRegisterFragmentInteractor.findTasks(getMainCondition(), currentLocation, getOperationalAreaCenter(), this.getString(R.string.house));
    }


}
