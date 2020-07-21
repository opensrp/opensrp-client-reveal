package org.smartregister.reveal.view;

import android.content.Context;
import android.view.View;
import android.widget.Button;

import androidx.appcompat.widget.Toolbar;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.mapboxsdk.maps.MapboxMap;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.presenter.EditFociBoundaryPresenter;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;

import io.ona.kujaku.layers.FillBoundaryLayer;
import io.ona.kujaku.manager.DrawingManager;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 7/20/20.
 */

public class EditFociBoundaryActivityTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private FillBoundaryLayer boundaryLayer;

    @Mock
    private DrawingManager drawingManager;

    @Mock
    private MapboxMap mapboxMap;

    @Mock
    private Feature feature;

    private Context context = RuntimeEnvironment.application;

    private EditFociBoundaryActivity editFociBoundaryActivity;

    @Before
    public void setUp() {
        FeatureCollection featureCollection = FeatureCollection.fromFeature(feature);
        RevealApplication.getInstance().setFeatureCollection(featureCollection);
        RevealApplication.getInstance().setOperationalArea(TestingUtils.getStructure());
        org.smartregister.Context.bindtypes = new ArrayList<>();
        editFociBoundaryActivity = Robolectric.buildActivity(EditFociBoundaryActivity.class).create().resume().get();
        Whitebox.setInternalState(editFociBoundaryActivity, "drawingManager", drawingManager);
        Whitebox.setInternalState(editFociBoundaryActivity, "boundaryLayer", boundaryLayer);
    }

    @Test
    public void testOnCreate() {
        Assert.assertNotNull(editFociBoundaryActivity);
    }

    @Test
    public void testSetToolbarTitle() {
        Toolbar toolbar = Whitebox.getInternalState(editFociBoundaryActivity, "toolbar");
        assertEquals("Edit Boundary", toolbar.getTitle());
        editFociBoundaryActivity.setToolbarTitle(R.string.change_point);
        assertEquals("Change Point", toolbar.getTitle());
    }

    @Test
    public void testDeletePoint() {
        View view = new View(context);
        assertTrue(view.isEnabled());
        editFociBoundaryActivity.deletePoint(view);
        verify(drawingManager).deleteDrawingCurrentCircle();
        assertFalse(view.isEnabled());
    }

    @Test
    public void testDeletePointWhenDrawingManagerIsNull() {
        Whitebox.setInternalState(editFociBoundaryActivity, "drawingManager", (Object[]) null);
        View view = new View(context);
        assertTrue(view.isEnabled());
        editFociBoundaryActivity.deletePoint(view);
        assertTrue(view.isEnabled());
    }

    @Test
    public void testExitEditBoundaryActivity() {
        when(drawingManager.isDrawingEnabled()).thenReturn(true);
        assertFalse(RevealApplication.getInstance().isRefreshMapOnEventSaved());
        editFociBoundaryActivity = spy(editFociBoundaryActivity);

        editFociBoundaryActivity.exitEditBoundaryActivity();
        verify(drawingManager).stopDrawingAndDisplayLayer();
        assertTrue(RevealApplication.getInstance().isRefreshMapOnEventSaved());
        verify(editFociBoundaryActivity).finish();
    }

    @Test
    public void testEnableDrawingModeWhenDrawingIsNotEnabled() {
        when(drawingManager.startDrawing(boundaryLayer)).thenReturn(true);
        Button savePointBtn = Whitebox.getInternalState(editFociBoundaryActivity, "savePointBtn");
        savePointBtn.setText(R.string.change_point);
        assertEquals("Change Point", savePointBtn.getText());

        editFociBoundaryActivity.enableDrawingMode(mapboxMap);
        verify(boundaryLayer).disableLayerOnMap(mapboxMap);
        assertEquals("Save Point", savePointBtn.getText());
    }

    @Test
    public void testEnableDrawingModeWhenDrawingIsEnabled() {
        when(drawingManager.isDrawingEnabled()).thenReturn(true);

        editFociBoundaryActivity.enableDrawingMode(mapboxMap);
        verify(boundaryLayer).disableLayerOnMap(mapboxMap);
        verify(drawingManager).stopDrawingAndDisplayLayer();
    }

}
