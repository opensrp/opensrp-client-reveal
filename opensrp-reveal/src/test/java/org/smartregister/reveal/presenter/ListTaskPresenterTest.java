package org.smartregister.reveal.presenter;

import android.graphics.PointF;
import android.graphics.RectF;
import androidx.appcompat.app.AlertDialog;
import android.widget.TextView;

import com.google.gson.JsonPrimitive;
import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.mapboxsdk.camera.CameraPosition;
import com.mapbox.mapboxsdk.geometry.LatLng;
import com.mapbox.mapboxsdk.maps.MapboxMap;
import com.mapbox.mapboxsdk.maps.Projection;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.shadows.ShadowAlertDialog;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Task;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.FamilyCardDetails;
import org.smartregister.reveal.model.IRSVerificationCardDetails;
import org.smartregister.reveal.model.MosquitoHarvestCardDetails;
import org.smartregister.reveal.model.SprayCardDetails;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.Filter;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Constants.InterventionType;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.UUID;

import static android.content.DialogInterface.BUTTON_NEGATIVE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.domain.Task.TaskStatus.IN_PROGRESS;
import static org.smartregister.reveal.util.Constants.BusinessStatus.BEDNET_DISTRIBUTED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_SPRAYED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.Intervention.PAOT;
import static org.smartregister.reveal.util.Constants.Intervention.REGISTER_FAMILY;
import static org.smartregister.reveal.util.Constants.Properties.FAMILY_MEMBER_NAMES;
import static org.smartregister.reveal.util.Constants.Properties.FEATURE_SELECT_TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.STRUCTURE_NAME;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;
import static org.smartregister.reveal.util.Constants.Properties.TASK_STATUS;
import static org.smartregister.reveal.util.Constants.REGISTER_STRUCTURE_EVENT;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;

/**
 * Created by samuelgithengi on 1/27/20.
 */
public class ListTaskPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private ListTaskPresenter listTaskPresenter;

    @Mock
    private ListTaskContract.ListTaskView listTaskView;

    @Mock
    private BaseDrawerContract.Presenter drawerPresenter;

    @Mock
    private ListTaskInteractor listTaskInteractor;

    @Mock
    private BaseDrawerContract.View drawerView;

    @Mock
    private MapboxMap mapboxMap;

    @Mock
    private Projection mapProjection;

    @Mock
    private Feature feature;

    @Mock
    private RevealJsonFormUtils jsonFormUtils;

    @Captor
    private ArgumentCaptor<FeatureCollection> featureCollectionArgumentCaptor;

    @Captor
    private ArgumentCaptor<Feature> featureArgumentCaptor;

    @Captor
    private ArgumentCaptor<Boolean> booleanArgumentCaptor;

    @Captor
    private ArgumentCaptor<String> stringArgumentCaptor;

    @Captor
    private ArgumentCaptor<CardDetails> cardDetailsArgumentCaptor;

    @Captor
    private ArgumentCaptor<CommonPersonObjectClient> commonPersonObjectClientArgumentCaptor;

    private PreferencesUtil prefsUtil = PreferencesUtil.getInstance();

    private String planId = UUID.randomUUID().toString();

    private String operationalArea = UUID.randomUUID().toString();


    private TaskFilterParams filterParams = TestingUtils.getFilterParams();

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        listTaskPresenter = new ListTaskPresenter(listTaskView, drawerPresenter);
        Whitebox.setInternalState(listTaskPresenter, "listTaskInteractor", listTaskInteractor);
        Whitebox.setInternalState(listTaskPresenter, "jsonFormUtils", jsonFormUtils);
        prefsUtil.setCurrentPlanId(planId);
        prefsUtil.setCurrentOperationalArea(operationalArea);
        when(listTaskView.getContext()).thenReturn(RuntimeEnvironment.application);
    }

    @Test
    public void testOnDrawerClosed() {
        when(drawerPresenter.isChangedCurrentSelection()).thenReturn(true);
        listTaskPresenter.onDrawerClosed();
        verify(listTaskView).showProgressDialog(R.string.fetching_structures_title, R.string.fetching_structures_message);
        verify(listTaskInteractor).fetchLocations(planId, operationalArea);
    }

    @Test
    public void testOnStructuresFetchedWithNoStructures() throws JSONException {
        JSONObject features = new JSONObject();
        features.put(Constants.GeoJSON.TYPE, Constants.GeoJSON.FEATURE_COLLECTION);
        listTaskPresenter.onStructuresFetched(features, feature, new ArrayList<>());
        verify(listTaskView).displayNotification(R.string.fetching_structures_title,
                R.string.fetch_location_and_structures_failed, operationalArea);

        verify(listTaskView).setGeoJsonSource(FeatureCollection.fromJson(features.toString()), feature, false);
        verify(listTaskView).clearSelectedFeature();
        verify(listTaskView).closeCardView(R.id.btn_collapse_spray_card_view);
        verify(drawerPresenter).setChangedCurrentSelection(false);
    }


    @Test
    public void testOnStructuresFetchedWithEmptyFeatures() throws JSONException {
        JSONObject features = new com.cocoahero.android.geojson.FeatureCollection().toJSON();
        listTaskPresenter.onStructuresFetched(features, feature, new ArrayList<>());
        verify(listTaskView).displayNotification(R.string.fetching_structures_title, R.string.no_structures_found);

        verify(listTaskView).setGeoJsonSource(FeatureCollection.fromJson(features.toString()), feature, false);
        verify(listTaskView, never()).clearSelectedFeature();
        verify(listTaskView, never()).closeCardView(R.id.btn_collapse_spray_card_view);
    }


    @Test
    public void testOnStructuresFetched() throws JSONException {
        FeatureCollection featureCollection = FeatureCollection.fromFeature(TestingUtils.getStructure());
        listTaskPresenter.onStructuresFetched(new JSONObject(featureCollection.toJson()), feature, Collections.singletonList(TestingUtils.getTaskDetails()));
        verify(drawerPresenter).setChangedCurrentSelection(false);
        verify(listTaskView).setGeoJsonSource(featureCollection, feature, false);
        assertEquals(feature, Whitebox.getInternalState(listTaskPresenter, "operationalArea"));
        assertFalse(Whitebox.getInternalState(listTaskPresenter, "isTasksFiltered"));

    }

    @Test
    public void testOnStructuresFetchedWithFilterAndNoSearchFiltersStructures() throws JSONException {
        FeatureCollection featureCollection = FeatureCollection.fromFeature(TestingUtils.getStructure());
        TaskFilterParams params = TestingUtils.getFilterParams();
        Whitebox.setInternalState(listTaskPresenter, "filterParams", params);
        listTaskPresenter.onStructuresFetched(new JSONObject(featureCollection.toJson()), feature, Collections.singletonList(TestingUtils.getTaskDetails()));
        verify(drawerPresenter).setChangedCurrentSelection(false);
        verify(listTaskView).setGeoJsonSource(FeatureCollection.fromFeatures(new ArrayList<>()), null, false);
        assertEquals(feature, Whitebox.getInternalState(listTaskPresenter, "operationalArea"));
        assertTrue(Whitebox.getInternalState(listTaskPresenter, "isTasksFiltered"));
        verify(listTaskView).setNumberOfFilters(params.getCheckedFilters().size());
        verify(listTaskView).setSearchPhrase("");
    }


    @Test
    public void testOnStructuresFetchedWithFilterAndSearchSearchesStructures() throws JSONException {
        FeatureCollection featureCollection = FeatureCollection.fromFeature(TestingUtils.getStructure());
        TaskFilterParams params = TestingUtils.getFilterParams();
        Whitebox.setInternalState(listTaskPresenter, "filterParams", params);
        Whitebox.setInternalState(listTaskPresenter, "searchPhrase", "Doe");
        listTaskPresenter.onStructuresFetched(new JSONObject(featureCollection.toJson()), feature, Collections.singletonList(TestingUtils.getTaskDetails()));
        verify(drawerPresenter).setChangedCurrentSelection(false);
        verify(listTaskView).setGeoJsonSource(FeatureCollection.fromFeatures(new ArrayList<>()), null, false);
        assertEquals(feature, Whitebox.getInternalState(listTaskPresenter, "operationalArea"));
    }

    @Test
    public void testOnMapReady() {
        listTaskPresenter.onMapReady();
        verify(listTaskInteractor).fetchLocations(planId, operationalArea);

    }

    @Test
    public void testOnMapReadyWithNoPlanOrOprationalAreaSelected() {
        when(drawerPresenter.getView()).thenReturn(drawerView);
        prefsUtil.setCurrentOperationalArea("");
        listTaskPresenter.onMapReady();
        verify(listTaskView).displayNotification(R.string.select_campaign_operational_area_title, R.string.select_campaign_operational_area);
        verify(drawerView).lockNavigationDrawerForSelection();

    }

    @Test
    public void testOnFilterTasksClicked() {
        Whitebox.setInternalState(listTaskPresenter, "filterParams", filterParams);
        listTaskPresenter.onFilterTasksClicked();
        verify(listTaskView).openFilterTaskActivity(filterParams);
    }

    @Test
    public void testOnOpenTaskRegisterClicked() {
        Whitebox.setInternalState(listTaskPresenter, "filterParams", filterParams);
        listTaskPresenter.onOpenTaskRegisterClicked();
        verify(listTaskView).openTaskRegister(filterParams);
    }

    @Test
    public void setTaskFilterParams() {
        TaskFilterParams params = new TaskFilterParams("Doe");
        listTaskPresenter.setTaskFilterParams(params);
        verify(listTaskView).setSearchPhrase(params.getSearchPhrase());
    }

    @Test
    public void testOnMapClickedWithInvalidZoomLevel() {
        CameraPosition cameraPosition = new CameraPosition.Builder().zoom(10).build();
        when(mapboxMap.getCameraPosition()).thenReturn(cameraPosition);
        listTaskPresenter.onMapClicked(mapboxMap, new LatLng(), false);
        verify(listTaskView).displayToast(R.string.zoom_in_to_select);
    }

    @Test
    public void testOnMapClickedMatches1Feature() {
        LatLng clickedPoint = new LatLng(12.06766, -18.02341);
        CameraPosition cameraPosition = new CameraPosition.Builder().zoom(19).build();
        when(mapboxMap.getCameraPosition()).thenReturn(cameraPosition);
        when(mapboxMap.getProjection()).thenReturn(mapProjection);
        when(mapProjection.toScreenLocation(clickedPoint)).thenReturn(new PointF());
        when(mapboxMap.queryRenderedFeatures(any(PointF.class), any())).thenReturn(new ArrayList<>());
        when(mapboxMap.queryRenderedFeatures(any(RectF.class), any())).thenReturn(Collections.singletonList(feature));
        when(listTaskView.getContext()).thenReturn(RuntimeEnvironment.application);
        listTaskPresenter.onMapClicked(mapboxMap, clickedPoint, false);
        verify(listTaskView).closeAllCardViews();
        verify(listTaskView).displaySelectedFeature(feature, clickedPoint);
    }

    @Test
    public void testOnMapClickedMatchesMultipleFeature() {
        LatLng clickedPoint = new LatLng(12.06766, -18.02341);
        CameraPosition cameraPosition = new CameraPosition.Builder().zoom(19).build();
        when(mapboxMap.getCameraPosition()).thenReturn(cameraPosition);
        when(mapboxMap.getProjection()).thenReturn(mapProjection);
        when(mapProjection.toScreenLocation(clickedPoint)).thenReturn(new PointF());
        Feature structure = TestingUtils.getStructure();
        when(mapboxMap.queryRenderedFeatures(any(PointF.class), any())).thenReturn(Arrays.asList(structure, feature));
        when(listTaskView.getContext()).thenReturn(RuntimeEnvironment.application);
        listTaskPresenter.onMapClicked(mapboxMap, clickedPoint, false);
        verify(listTaskView).closeAllCardViews();
        verify(listTaskView).displaySelectedFeature(structure, clickedPoint);
    }

    @Test
    public void testFilterTasksWithNullParams() {
        listTaskPresenter.filterTasks(new TaskFilterParams(""));
        verify(listTaskView).setNumberOfFilters(0);
        assertFalse(Whitebox.getInternalState(listTaskPresenter, "isTasksFiltered"));
    }

    @Test
    public void testFilterTasksBusinessStatus() {
        Feature structure = TestingUtils.getStructure();
        TaskFilterParams params = new TaskFilterParams("", new HashMap<>());
        params.getCheckedFilters().put(Filter.STATUS, Collections.singleton(NOT_VISITED));
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", FeatureCollection.fromFeature(structure));
        //match is filter for business status works no feature is returned
        listTaskPresenter.filterTasks(params);
        verify(listTaskView).setGeoJsonSource(FeatureCollection.fromFeatures(new ArrayList<>()), null, false);
        assertTrue(Whitebox.getInternalState(listTaskPresenter, "isTasksFiltered"));


        //match is filter for business status works feature is returned
        structure.addStringProperty(Constants.Properties.TASK_BUSINESS_STATUS, NOT_VISITED);
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", FeatureCollection.fromFeatures(new Feature[]{structure, feature}));
        listTaskPresenter.filterTasks(params);
        verify(listTaskView).setGeoJsonSource(FeatureCollection.fromFeature(structure), null, false);
        assertTrue(Whitebox.getInternalState(listTaskPresenter, "isTasksFiltered"));


    }


    @Test
    public void testFilterWithAllFilters() {
        Feature structure = TestingUtils.getStructure();
        TaskFilterParams params = new TaskFilterParams("", new HashMap<>());
        params.getCheckedFilters().put(Filter.STATUS, Collections.singleton(NOT_VISITED));
        params.getCheckedFilters().put(Filter.CODE, Collections.singleton(Intervention.IRS));
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", FeatureCollection.fromFeature(structure));
        //match is filter for business status works no feature is returned
        listTaskPresenter.filterTasks(params);
        verify(listTaskView).setGeoJsonSource(FeatureCollection.fromFeatures(new ArrayList<>()), null, false);
        assertTrue(Whitebox.getInternalState(listTaskPresenter, "isTasksFiltered"));


        //match is filter for business status works feature is returned
        structure.addStringProperty(Constants.Properties.TASK_BUSINESS_STATUS, NOT_VISITED);
        structure.addProperty(Constants.Properties.TASK_CODE_LIST, new JsonPrimitive(Intervention.IRS));
        structure.addStringProperty(Constants.Properties.TASK_CODE, Intervention.IRS);

        Whitebox.setInternalState(listTaskPresenter, "featureCollection", FeatureCollection.fromFeatures(new Feature[]{structure, feature}));
        listTaskPresenter.filterTasks(params);
        verify(listTaskView).setGeoJsonSource(FeatureCollection.fromFeature(structure), null, false);
        assertTrue(Whitebox.getInternalState(listTaskPresenter, "isTasksFiltered"));

        //filter by intervention unit structure
        params.getCheckedFilters().put(Filter.INTERVENTION_UNIT, Collections.singleton(InterventionType.STRUCTURE));
        listTaskPresenter.filterTasks(params);
        verify(listTaskView, times(2)).setGeoJsonSource(FeatureCollection.fromFeature(structure), null, false);

        //no features are returned with wrong intervention unit
        params.getCheckedFilters().put(Filter.INTERVENTION_UNIT, Collections.singleton(InterventionType.PERSON));
        listTaskPresenter.filterTasks(params);
        verify(listTaskView, times(2)).setGeoJsonSource(FeatureCollection.fromFeatures(new ArrayList<>()), null, false);


    }


    @Test
    public void testSearchTasksWithEmptyPhrase() {
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", FeatureCollection.fromFeatures(new Feature[]{feature}));
        listTaskPresenter.searchTasks("");
        verify(listTaskView).setGeoJsonSource(FeatureCollection.fromFeatures(new Feature[]{feature}), null, false);
    }

    @Test
    public void testSearchTasks() throws JSONException {
        com.cocoahero.android.geojson.Feature feature1 = new com.cocoahero.android.geojson.Feature();
        feature1.setIdentifier("id1");
        feature1.setProperties(new JSONObject().accumulate(STRUCTURE_NAME, "John Doe House"));

        com.cocoahero.android.geojson.Feature feature2 = new com.cocoahero.android.geojson.Feature();
        feature2.setIdentifier("id2");
        feature2.setProperties(new JSONObject().accumulate(FAMILY_MEMBER_NAMES, "John Doe,Jane Doe,Helli Pad"));


        Whitebox.setInternalState(listTaskPresenter, "featureCollection",
                FeatureCollection.fromFeatures(new Feature[]{Feature.fromJson(feature1.toJSON().toString()),
                        Feature.fromJson(feature2.toJSON().toString())}));

        listTaskPresenter.searchTasks("Doe");
        verify(listTaskView).setGeoJsonSource(featureCollectionArgumentCaptor.capture(), eq(null), eq(false));
        assertEquals(2, featureCollectionArgumentCaptor.getValue().features().size());

        listTaskPresenter.searchTasks("House");
        verify(listTaskView, times(2)).setGeoJsonSource(featureCollectionArgumentCaptor.capture(), eq(null), eq(false));
        assertEquals(1, featureCollectionArgumentCaptor.getValue().features().size());
        assertEquals("id1", featureCollectionArgumentCaptor.getValue().features().get(0).id());


        listTaskPresenter.searchTasks("Helli");
        verify(listTaskView, times(3)).setGeoJsonSource(featureCollectionArgumentCaptor.capture(), eq(null), eq(false));
        assertEquals(1, featureCollectionArgumentCaptor.getValue().features().size());
        assertEquals("id2", featureCollectionArgumentCaptor.getValue().features().get(0).id());
    }

    @Test
    public void testOnInterventionTaskInfoReset() {
        RevealApplication.getInstance().setRefreshMapOnEventSaved(true);
        listTaskPresenter.onInterventionTaskInfoReset(true);

        verify(listTaskView).hideProgressDialog();
        verify(listTaskView).clearSelectedFeature();
        assertFalse(RevealApplication.getInstance().isRefreshMapOnEventSaved());
    }

    @Test
    public void testOnstructureMarkedInactive() throws Exception {

        com.cocoahero.android.geojson.Feature feature1 = new com.cocoahero.android.geojson.Feature();
        feature1.setIdentifier("id1");
        feature1.setProperties(new JSONObject()
                .accumulate(TASK_BUSINESS_STATUS, BEDNET_DISTRIBUTED)
                .accumulate(TASK_IDENTIFIER, "task1"));


        Feature mapboxFeature = Feature.fromJson(feature1.toJSON().toString());
        assertEquals(BEDNET_DISTRIBUTED, mapboxFeature.getStringProperty(TASK_BUSINESS_STATUS));
        assertEquals("task1", mapboxFeature.getStringProperty(TASK_IDENTIFIER));

        Whitebox.setInternalState(listTaskPresenter, "featureCollection",
                FeatureCollection.fromFeatures(new Feature[]{mapboxFeature}));

        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mapboxFeature);

        listTaskPresenter.onStructureMarkedInactive();
        verify(listTaskView).setGeoJsonSource(featureCollectionArgumentCaptor.capture(), any(), booleanArgumentCaptor.capture());
        assertFalse(booleanArgumentCaptor.getValue());
        assertNull(featureCollectionArgumentCaptor.getValue().features().get(0).getStringProperty(TASK_BUSINESS_STATUS));
        assertNull(featureCollectionArgumentCaptor.getValue().features().get(0).getStringProperty(TASK_IDENTIFIER));
    }

    @Test
    public void testOnMarkStructureInEligibleConfirmed() throws Exception {

        com.cocoahero.android.geojson.Feature feature1 = new com.cocoahero.android.geojson.Feature();
        feature1.setIdentifier("id1");
        Feature mapboxFeature = Feature.fromJson(feature1.toJSON().toString());
        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mapboxFeature);
        Whitebox.setInternalState(listTaskPresenter, "reasonUnEligible", "No residents");
        listTaskPresenter.onMarkStructureIneligibleConfirmed();

        verify(listTaskInteractor).markStructureAsIneligible(featureArgumentCaptor.capture(), stringArgumentCaptor.capture());
        assertEquals("No residents", stringArgumentCaptor.getValue());
        assertEquals(mapboxFeature.id(), featureArgumentCaptor.getValue().id());
    }

    @Test
    public void testOnStructureMarkedIneligible() throws Exception {
        com.cocoahero.android.geojson.Feature feature1 = new com.cocoahero.android.geojson.Feature();
        feature1.setIdentifier("id1");
        feature1.setProperties(new JSONObject());


        Feature mapboxFeature = Feature.fromJson(feature1.toJSON().toString());
        assertNull(mapboxFeature.getStringProperty(TASK_BUSINESS_STATUS));
        assertNull(mapboxFeature.getStringProperty(TASK_IDENTIFIER));

        Whitebox.setInternalState(listTaskPresenter, "featureCollection",
                FeatureCollection.fromFeatures(new Feature[]{mapboxFeature}));

        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mapboxFeature);

        listTaskPresenter.onStructureMarkedIneligible();
        verify(listTaskView).setGeoJsonSource(featureCollectionArgumentCaptor.capture(), any(), booleanArgumentCaptor.capture());
        assertFalse(booleanArgumentCaptor.getValue());
        assertEquals(NOT_ELIGIBLE, featureCollectionArgumentCaptor.getValue().features().get(0).getStringProperty(TASK_BUSINESS_STATUS));
        assertEquals(NOT_ELIGIBLE, featureCollectionArgumentCaptor.getValue().features().get(0).getStringProperty(FEATURE_SELECT_TASK_BUSINESS_STATUS));
    }

    @Test
    public void testOnFamilyFound() {
        CommonPersonObjectClient family = new CommonPersonObjectClient("caseId", null, "test family");

        listTaskPresenter.onFamilyFound(family);

        verify(listTaskView).openStructureProfile(commonPersonObjectClientArgumentCaptor.capture());
        assertEquals("caseId", commonPersonObjectClientArgumentCaptor.getValue().getCaseId());
        assertEquals("test family", commonPersonObjectClientArgumentCaptor.getValue().getName());
    }

    @Test
    public void testOnFamilyFoundWithNullParam() {

        listTaskPresenter.onFamilyFound(null);
        verify(listTaskView).displayNotification(R.string.fetch_family_failed, R.string.failed_to_find_family);
    }

    @Test
    public void testOnResume() {
        RevealApplication.getInstance().setRefreshMapOnEventSaved(true);
        listTaskPresenter = spy(listTaskPresenter);
        listTaskPresenter.onResume();

        verify(listTaskPresenter).refreshStructures(true);
        verify(listTaskView).clearSelectedFeature();
        assertFalse(RevealApplication.getInstance().isRefreshMapOnEventSaved());
    }

    @Test
    public void testDisplayMarkStructureIneligibleDialog() {

        Whitebox.setInternalState(listTaskPresenter, "markStructureIneligibleConfirmed", false);
        Whitebox.setInternalState(listTaskPresenter, "reasonUnEligible", "eligible");

        assertFalse(Whitebox.getInternalState(listTaskPresenter, "markStructureIneligibleConfirmed"));
        assertEquals("eligible", Whitebox.getInternalState(listTaskPresenter, "reasonUnEligible"));
        listTaskPresenter.displayMarkStructureIneligibleDialog();

        AlertDialog alertDialog = (AlertDialog) ShadowAlertDialog.getLatestDialog();
        assertTrue(alertDialog.isShowing());

        TextView tv = alertDialog.findViewById(android.R.id.message);
        assertEquals(getString(R.string.is_structure_eligible_for_fam_reg), tv.getText());

        alertDialog.getButton(BUTTON_NEGATIVE).performClick();
        assertFalse(alertDialog.isShowing());

        assertTrue(Whitebox.getInternalState(listTaskPresenter, "markStructureIneligibleConfirmed"));
        assertEquals(listTaskView.getContext().getString(R.string.not_eligible_unoccupied),
                Whitebox.getInternalState(listTaskPresenter, "reasonUnEligible"));

    }

    @Test
    public void testOnUndoInterventionStatus() throws Exception {

        com.cocoahero.android.geojson.Feature feature1 = new com.cocoahero.android.geojson.Feature();
        feature1.setIdentifier("id1");


        Feature mapboxFeature = Feature.fromJson(feature1.toJSON().toString());


        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mapboxFeature);

        listTaskPresenter.onUndoInterventionStatus(BLOOD_SCREENING);

        verify(listTaskView).showProgressDialog(R.string.reseting_task_title, R.string.reseting_task_msg);
        verify(listTaskInteractor).resetInterventionTaskInfo(BLOOD_SCREENING, mapboxFeature.id());

    }

    @Test
    public void testSaveJsonForm() {
        String jsonString = "{\"name\":\"trever\"}";

        listTaskPresenter.saveJsonForm(jsonString);
        verify(listTaskView).showProgressDialog(R.string.saving_title, R.string.saving_message);
        verify(listTaskInteractor).saveJsonForm(jsonString);
    }

    @Test
    public void testOnFormSaved() throws Exception{
        String structureId = "id1";
        String taskId = "taskId";

        com.cocoahero.android.geojson.Feature feature1 = new com.cocoahero.android.geojson.Feature();
        feature1.setIdentifier(structureId);
        feature1.setProperties(new JSONObject());


        Feature mapboxFeature = Feature.fromJson(feature1.toJSON().toString());
        assertNull(mapboxFeature.getStringProperty(TASK_BUSINESS_STATUS));
        assertNull(mapboxFeature.getStringProperty(TASK_STATUS));

        Whitebox.setInternalState(listTaskPresenter, "featureCollection",
                FeatureCollection.fromFeatures(new Feature[]{mapboxFeature}));

        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mapboxFeature);

        listTaskPresenter.onFormSaved(structureId, taskId, IN_PROGRESS, COMPLETE, BLOOD_SCREENING);

        verify(listTaskView).hideProgressDialog();

        verify(listTaskView).setGeoJsonSource(featureCollectionArgumentCaptor.capture(), any(), booleanArgumentCaptor.capture());
        assertFalse(booleanArgumentCaptor.getValue());
        assertEquals(COMPLETE, featureCollectionArgumentCaptor.getValue().features().get(0).getStringProperty(TASK_BUSINESS_STATUS));
        assertEquals(IN_PROGRESS.name(), featureCollectionArgumentCaptor.getValue().features().get(0).getStringProperty(TASK_STATUS));

        verify(listTaskInteractor).fetchInterventionDetails(BLOOD_SCREENING, structureId, false);
    }

    @Test
    public void testResetFeatureTasks() throws Exception {
        String structureId = "id1";
        com.cocoahero.android.geojson.Feature feature1 = new com.cocoahero.android.geojson.Feature();
        feature1.setIdentifier(structureId);
        feature1.setProperties(new JSONObject());


        Feature mapboxFeature = Feature.fromJson(feature1.toJSON().toString());
        assertNull(mapboxFeature.getStringProperty(TASK_BUSINESS_STATUS));
        assertNull(mapboxFeature.getStringProperty(TASK_STATUS));
        assertNull(mapboxFeature.getStringProperty(TASK_IDENTIFIER));
        assertNull(mapboxFeature.getStringProperty(TASK_CODE));
        assertNull(mapboxFeature.getStringProperty(FEATURE_SELECT_TASK_BUSINESS_STATUS));

        Whitebox.setInternalState(listTaskPresenter, "featureCollection",
                FeatureCollection.fromFeatures(new Feature[]{mapboxFeature}));

        Task task = TestingUtils.getTask("entityid");

        listTaskPresenter.resetFeatureTasks(structureId, task);

        verify(listTaskView).setGeoJsonSource(featureCollectionArgumentCaptor.capture(), any(), booleanArgumentCaptor.capture());
        assertFalse(booleanArgumentCaptor.getValue());
        assertEquals(task.getBusinessStatus(), featureCollectionArgumentCaptor.getValue().features().get(0).getStringProperty(TASK_BUSINESS_STATUS));
        assertEquals(task.getBusinessStatus(), featureCollectionArgumentCaptor.getValue().features().get(0).getStringProperty(FEATURE_SELECT_TASK_BUSINESS_STATUS));
        assertEquals(task.getStatus().name(), featureCollectionArgumentCaptor.getValue().features().get(0).getStringProperty(TASK_STATUS));
        assertEquals(task.getIdentifier(), featureCollectionArgumentCaptor.getValue().features().get(0).getStringProperty(TASK_IDENTIFIER));
        assertEquals(task.getCode(), featureCollectionArgumentCaptor.getValue().features().get(0).getStringProperty(TASK_CODE));

    }

    @Test
    public void testOnFormSaveFailureForSprayEvent() {

        listTaskPresenter.onFormSaveFailure(SPRAY_EVENT);

        verify(listTaskView).hideProgressDialog();
        verify(listTaskView).displayNotification(R.string.form_save_failure_title, R.string.spray_form_save_failure);
    }

    @Test
    public void testOnFormSaveFailureForStructureEvent() {

        listTaskPresenter.onFormSaveFailure(REGISTER_STRUCTURE_EVENT);

        verify(listTaskView).hideProgressDialog();
        verify(listTaskView).displayNotification(R.string.form_save_failure_title, R.string.add_structure_form_save_failure);
    }

    @Test
    public void testOnInterventionFormDetailsFetched() {
        assertNull(Whitebox.getInternalState(listTaskPresenter, "cardDetails"));
        assertFalse(Whitebox.getInternalState(listTaskPresenter, "changeInterventionStatus"));

        FamilyCardDetails expectedCardDetails = new FamilyCardDetails(COMPLETE, "12-2-2020", "nifi-user");

        listTaskPresenter.onInterventionFormDetailsFetched(expectedCardDetails);

        verify(listTaskView).hideProgressDialog();
        assertTrue(Whitebox.getInternalState(listTaskPresenter, "changeInterventionStatus"));

        FamilyCardDetails actualCardDetails = Whitebox.getInternalState(listTaskPresenter, "cardDetails");
        assertEquals(expectedCardDetails.getStatus(), actualCardDetails.getStatus());
        assertEquals(expectedCardDetails.getDateCreated(), actualCardDetails.getDateCreated());
        assertEquals(expectedCardDetails.getOwner(), actualCardDetails.getOwner());

    }

    @Test
    public void testOnFamilyCardDetailsFetched() {

        FamilyCardDetails expectedCardDetails = new FamilyCardDetails(COMPLETE, "1582279044", "nifi-user");

        listTaskPresenter.onCardDetailsFetched(expectedCardDetails);

        verify(listTaskView).openCardView(cardDetailsArgumentCaptor.capture());

        FamilyCardDetails actualCardDetails = (FamilyCardDetails) cardDetailsArgumentCaptor.getValue();

        assertEquals(COMPLETE, actualCardDetails.getStatus());
        assertEquals("nifi-user", actualCardDetails.getOwner());
        assertEquals("19 Jan 1970", actualCardDetails.getDateCreated());


    }

    @Test
    public void testOnSprayCardDetailsFetched() {

        SprayCardDetails expectedCardDetails = new SprayCardDetails(NOT_SPRAYED, "Residential", "2014-07-04T12:08:56.235-0700", "gideon", "Mark", "Available");

        listTaskPresenter.onCardDetailsFetched(expectedCardDetails);

        verify(listTaskView).openCardView(cardDetailsArgumentCaptor.capture());

        SprayCardDetails actualCardDetails = (SprayCardDetails) cardDetailsArgumentCaptor.getValue();

        assertEquals(NOT_SPRAYED, actualCardDetails.getStatus());
        assertEquals("Residential", actualCardDetails.getPropertyType());
        assertEquals("04 Jul 2014", actualCardDetails.getSprayDate());
        assertEquals("gideon", actualCardDetails.getSprayOperator());
        assertEquals("Mark", actualCardDetails.getFamilyHead());
        assertEquals("Available", actualCardDetails.getReason());

    }

    @Test
    public void testOnMosquitoHarvestCardDetailsFetched() {

        MosquitoHarvestCardDetails expectedCardDetails = new MosquitoHarvestCardDetails(NOT_VISITED, "2019-07-04", "2019-08-05", MOSQUITO_COLLECTION);


        listTaskPresenter.onCardDetailsFetched(expectedCardDetails);

        verify(listTaskView).openCardView(cardDetailsArgumentCaptor.capture());

        MosquitoHarvestCardDetails actualCardDetails = (MosquitoHarvestCardDetails) cardDetailsArgumentCaptor.getValue();

        assertEquals(NOT_VISITED, actualCardDetails.getStatus());
        assertEquals("2019-07-04", actualCardDetails.getStartDate());
        assertEquals("2019-08-05", actualCardDetails.getEndDate());
        assertEquals(MOSQUITO_COLLECTION, actualCardDetails.getInterventionType());
    }

    @Test
    public void testOnIRSVerificationCardDetailsFetched() {
        IRSVerificationCardDetails expectedCardDetails = new IRSVerificationCardDetails(NOT_VISITED,
                "yes", "no", "sprayed", "No chalk",
                "No sticker", "No card");


        listTaskPresenter.onCardDetailsFetched(expectedCardDetails);

        verify(listTaskView).openCardView(cardDetailsArgumentCaptor.capture());

        IRSVerificationCardDetails actualCardDetails = (IRSVerificationCardDetails) cardDetailsArgumentCaptor.getValue();

        assertEquals(NOT_VISITED, actualCardDetails.getStatus());
        assertEquals("yes", actualCardDetails.getTrueStructure());
        assertEquals("no", actualCardDetails.getEligStruc());
        assertEquals("sprayed", actualCardDetails.getReportedSprayStatus());
        assertEquals("No chalk", actualCardDetails.getChalkSprayStatus());
        assertEquals("No sticker", actualCardDetails.getStickerSprayStatus());
        assertEquals("No card", actualCardDetails.getCardSprayStatus());

    }

    @Test
    public void testOnLocationValidatedForMarkStructureIneligible() {

        Whitebox.setInternalState(listTaskPresenter, "markStructureIneligibleConfirmed", true);
        assertTrue(Whitebox.getInternalState(listTaskPresenter, "markStructureIneligibleConfirmed"));
        listTaskPresenter = spy(listTaskPresenter);
        listTaskPresenter.onLocationValidated();

        verify(listTaskPresenter).onMarkStructureIneligibleConfirmed();
        assertFalse(Whitebox.getInternalState(listTaskPresenter, "markStructureIneligibleConfirmed"));
    }

    @Test
    public void testOnLocationValidatedForRegisterFamily() {

        Whitebox.setInternalState(listTaskPresenter, "selectedFeatureInterventionType", REGISTER_FAMILY);

        listTaskPresenter.onLocationValidated();

        verify(listTaskView).registerFamily();
    }

    @Test
    public void testOnLocationValidatedForCardDetailsWithChangeInterventionStatusFalse() {

        Whitebox.setInternalState(listTaskPresenter, "selectedFeatureInterventionType", PAOT);
        Whitebox.setInternalState(listTaskPresenter, "changeInterventionStatus", false);
        listTaskPresenter = spy(listTaskPresenter);
        listTaskPresenter.onLocationValidated();

        verify(listTaskPresenter).startForm(featureArgumentCaptor.capture(), cardDetailsArgumentCaptor.capture(), stringArgumentCaptor.capture());
        assertNull(cardDetailsArgumentCaptor.getValue());
        assertEquals(PAOT, stringArgumentCaptor.getValue());
    }

    @Test
    public void testOnlocationValidatedForCardDetailWithChangeInterventionTrue() {
        FamilyCardDetails expectedCardDetails = new FamilyCardDetails(COMPLETE, "19 Jan 1970", "nifi-user");
        Whitebox.setInternalState(listTaskPresenter, "cardDetails", expectedCardDetails);
        Whitebox.setInternalState(listTaskPresenter, "selectedFeatureInterventionType", PAOT);
        Whitebox.setInternalState(listTaskPresenter, "changeInterventionStatus", true);
        listTaskPresenter = spy(listTaskPresenter);
        listTaskPresenter.onLocationValidated();

        verify(listTaskPresenter).startForm(featureArgumentCaptor.capture(), cardDetailsArgumentCaptor.capture(), stringArgumentCaptor.capture());
        assertEquals(PAOT, stringArgumentCaptor.getValue());

        FamilyCardDetails actualCardDetails = (FamilyCardDetails) cardDetailsArgumentCaptor.getValue();

        assertEquals(COMPLETE, actualCardDetails.getStatus());
        assertEquals("nifi-user", actualCardDetails.getOwner());
        assertEquals("19 Jan 1970", actualCardDetails.getDateCreated());
    }

    @Test
    public void testOnChangeInterventionStatusForIRS() throws JSONException {
        Feature mapboxFeature = initTestFeature("id1");

        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mapboxFeature);
        listTaskPresenter.onChangeInterventionStatus(IRS);
        verify(listTaskView).showProgressDialog(R.string.fetching_structure_title, R.string.fetching_structure_message);
        verify(listTaskInteractor).fetchInterventionDetails(IRS, "id1", true);
    }

    @Test
    public void testOnChangeInterventionStatusForMosquitoCollection() throws JSONException {

        Feature mapboxFeature = initTestFeature("id1");
        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mapboxFeature);
        listTaskPresenter.onChangeInterventionStatus(MOSQUITO_COLLECTION);
        verify(listTaskView).showProgressDialog(R.string.fetching_mosquito_collection_points_title, R.string.fetching_mosquito_collection_points_message);
        verify(listTaskInteractor).fetchInterventionDetails(MOSQUITO_COLLECTION, "id1", true);
    }

    @Test
    public void testOnChangeInterventionStatusForLarvalDipping() throws JSONException {

        Feature mapboxFeature = initTestFeature("id1");
        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mapboxFeature);
        listTaskPresenter.onChangeInterventionStatus(LARVAL_DIPPING);
        verify(listTaskView).showProgressDialog(R.string.fetching_larval_dipping_points_title, R.string.fetching_larval_dipping_points_message);
        verify(listTaskInteractor).fetchInterventionDetails(LARVAL_DIPPING, "id1", true);
    }

    @Test
    public void testOnChangeInterventionStatusForPAOT() throws JSONException {

        Feature mapboxFeature = initTestFeature("id1");
        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mapboxFeature);
        listTaskPresenter.onChangeInterventionStatus(PAOT);
        verify(listTaskView).showProgressDialog(R.string.fetching_paot_title, R.string.fetching_paot_message);
        verify(listTaskInteractor).fetchInterventionDetails(PAOT, "id1", true);
    }


    private Feature initTestFeature(String identifier) throws JSONException {
        String structureId = identifier;
        com.cocoahero.android.geojson.Feature feature1 = new com.cocoahero.android.geojson.Feature();
        feature1.setIdentifier(structureId);
        feature1.setProperties(new JSONObject());

        return Feature.fromJson(feature1.toJSON().toString());
    }

}
