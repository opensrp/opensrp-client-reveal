package org.smartregister.reveal.presenter;

import android.graphics.PointF;
import android.graphics.RectF;

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
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.Filter;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Constants.InterventionType;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.Properties.FAMILY_MEMBER_NAMES;
import static org.smartregister.reveal.util.Constants.Properties.STRUCTURE_NAME;

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

    @Captor
    private ArgumentCaptor<FeatureCollection> featureCollectionArgumentCaptor;

    private PreferencesUtil prefsUtil = PreferencesUtil.getInstance();

    private String planId = UUID.randomUUID().toString();

    private String operationalArea = UUID.randomUUID().toString();


    private TaskFilterParams filterParams = TestingUtils.getFilterParams();

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        listTaskPresenter = new ListTaskPresenter(listTaskView, drawerPresenter);
        Whitebox.setInternalState(listTaskPresenter, "listTaskInteractor", listTaskInteractor);
        prefsUtil.setCurrentPlanId(planId);
        prefsUtil.setCurrentOperationalArea(operationalArea);
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
        FeatureCollection featureCollection = FeatureCollection.fromFeature(TestingUtils.getStucture());
        listTaskPresenter.onStructuresFetched(new JSONObject(featureCollection.toJson()), feature, Collections.singletonList(TestingUtils.getTaskDetails()));
        verify(drawerPresenter).setChangedCurrentSelection(false);
        verify(listTaskView).setGeoJsonSource(featureCollection, feature, false);
        assertEquals(feature, Whitebox.getInternalState(listTaskPresenter, "operationalArea"));
        assertFalse(Whitebox.getInternalState(listTaskPresenter, "isTasksFiltered"));

    }

    @Test
    public void testOnStructuresFetchedWithFilterAndNoSearchFiltersStructures() throws JSONException {
        FeatureCollection featureCollection = FeatureCollection.fromFeature(TestingUtils.getStucture());
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
        FeatureCollection featureCollection = FeatureCollection.fromFeature(TestingUtils.getStucture());
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
        Feature structure = TestingUtils.getStucture();
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
        Feature structure = TestingUtils.getStucture();
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
        Feature structure = TestingUtils.getStucture();
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


}
