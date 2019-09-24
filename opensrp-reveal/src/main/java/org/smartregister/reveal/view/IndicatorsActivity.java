package org.smartregister.reveal.view;

import android.app.Activity;
import android.os.Bundle;
import android.support.annotation.NonNull;

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
import org.smartregister.reveal.util.DBQueryHelper;
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

            String key = tasks.get(i).getSprayStatus() == null ? Constants.NULL_KEY : tasks.get(i).getSprayStatus();

            int newValue = map.get(key) != null ? map.get(key) + 1 : 1;

            map.put(key, newValue);

        }

        int sprayed = map.get(org.smartregister.reveal.util.Constants.BusinessStatus.SPRAYED);
        int notSprayed = map.get(org.smartregister.reveal.util.Constants.BusinessStatus.NOT_SPRAYED);
        int totalStructures = tasks.size();
        int progress = totalStructures > 0 ? Math.round(sprayed * 100 / totalStructures) : 0;
        int notVisited = map.get(Constants.NULL_KEY) != null ? map.get(Constants.NULL_KEY) : 0;

        sprayIndicator.add(getResources().getString(R.string.spray_coverage));
        sprayIndicator.add(getResources().getString(R.string.n_percent, progress));

        int totalFound = (sprayed + notSprayed);

        sprayIndicator.add(getResources().getString(R.string.structures_remaining_90));
        sprayIndicator.add(String.valueOf(Math.round(totalStructures * 0.9) - sprayed));


        sprayIndicator.add(getResources().getString(R.string.total_structures));
        sprayIndicator.add(String.valueOf(totalStructures));


        sprayIndicator.add(getResources().getString(R.string.structures_not_visited));
        sprayIndicator.add(String.valueOf(notVisited));


        sprayIndicator.add(getResources().getString(R.string.structures_visited_found));
        sprayIndicator.add(String.valueOf(totalFound));


        sprayIndicator.add(getResources().getString(R.string.sprayed));
        sprayIndicator.add(String.valueOf(sprayed));


        sprayIndicator.add(getResources().getString(R.string.structures_not_sprayed));
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
        //Overriden
    }

    @Override
    public void onTaskSelected(TaskDetails details, boolean isActionClicked) {

        //Overriden
    }

    @Override
    public int getInterventionLabel() {
        return 0;
    }

    @Override
    public void onIndexCaseFound(JSONObject indexCase, boolean isLinkedToJurisdiction) {

        //Overriden
    }

    @Override
    public void onFormSaved(@NonNull String structureId, String taskID, @NonNull Task.TaskStatus taskStatus, @NonNull String businessStatus, String interventionType) {

        //Overriden
    }

    @Override
    public void onStructureAdded(Feature feature, JSONArray featureCoordinates, double zoomlevel) {

        //Overriden
    }

    @Override
    public void onFormSaveFailure(String eventType) {

        //Overriden
    }

    @Override
    public void onFamilyFound(CommonPersonObjectClient finalFamily) {

        //Overriden
    }

    @Override
    public void onStructureFound(Location structure, BaseTaskDetails details) {

        //Overriden
    }

    @Override
    public void onFetchedMembersCount(int numberOfMembers, JSONObject formJSON) {

        //Overriden
    }

    @Override
    public void onFetchedFamilyMembers(JSONArray familyMembers, JSONObject formJSON) {

        //Overriden
    }

    @Override
    public void onFetchedSprayDetails(CommonPersonObject commonPersonObject, JSONObject formJSON) {

    }

    @Override
    public void onPasswordVerified() {

        //Overriden
    }

    @Override
    public void onLocationValidated() {

        //Overriden
    }

    @Override
    public LatLng getTargetCoordinates() {
        return null;
    }

    @Override
    public void requestUserPassword() {

        //Overriden
    }

    @Override
    public ValidateUserLocationPresenter getLocationPresenter() {
        return null;
    }

    @Override
    public void processViewConfigurations() {

        //Overriden
    }

    @Override
    public void initializeQueries(String s) {

        //Overriden
    }

    @Override
    public void startSync() {

        //Overriden
    }

    @Override
    public void searchGlobally(String s) {

        //Overriden
    }

    private android.location.Location getOperationalAreaCenter() {
        org.smartregister.domain.Location operationalAreaLocation = org.smartregister.reveal.util.Utils.getOperationalAreaLocation(PreferencesUtil.getInstance().getCurrentOperationalArea());
        if (operationalAreaLocation == null)
            return null;
        return new RevealMappingHelper().getCenter(gson.toJson(operationalAreaLocation.getGeometry()));
    }

    private void fetchTaskDetails() {

        taskRegisterFragmentInteractor.findTasks(DBQueryHelper.getMainCondition(), currentLocation, getOperationalAreaCenter(), this.getString(R.string.house));
    }


}
