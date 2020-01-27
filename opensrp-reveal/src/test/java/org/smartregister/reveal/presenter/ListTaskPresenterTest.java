package org.smartregister.reveal.presenter;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.reveal.model.TaskFilterParams;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;
import java.util.UUID;

import edu.emory.mathcs.backport.java.util.Collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
    private Feature feature;


    private PreferencesUtil prefsUtil = PreferencesUtil.getInstance();

    private String planId = UUID.randomUUID().toString();

    private String operationalArea = UUID.randomUUID().toString();

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
        //TODO add assert that verify the filter works

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
        assertFalse(Whitebox.getInternalState(listTaskPresenter, "isTasksFiltered"));
        //TODO add assert that verify the search works


    }

}
