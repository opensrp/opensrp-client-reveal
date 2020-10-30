package org.smartregister.reveal.adapter;

import android.content.Context;
import android.util.Pair;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.model.LocationModel;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import edu.emory.mathcs.backport.java.util.Collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * Created by Richard Kareko on 10/30/20.
 */

public class ExpandableListViewAdapterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private LocationModel locationModel;

    private ExpandableListViewAdapter adapter;


    @Before
    public void setUp() {
        Context context = RuntimeEnvironment.application;
        List<Pair<String, String>>  listGroup  = Collections.singletonList(Pair.create("parent-location-id1", "Lusaka"));
        locationModel = new LocationModel();
        locationModel.setId("location-id1");
        locationModel.setName("Aksro-1");
        locationModel.setChecked(true);
        Map<String, List<LocationModel>> childLocationsMap = Collections.singletonMap("parent-location-id1", Collections.singletonList(locationModel));
        adapter = new ExpandableListViewAdapter(context, listGroup, childLocationsMap);
    }

    @Test
    public void testInitialization() {
        assertNotNull(Whitebox.getInternalState(adapter, "context"));
        assertNotNull(Whitebox.getInternalState(adapter, "listGroup"));
        assertNotNull(Whitebox.getInternalState(adapter, "childLocationsMap"));
    }

    @Test
    public void testGetGroupCount() {
        assertEquals(1, adapter.getGroupCount());
    }

    @Test
    public void testGetChildrenCount() {
        assertEquals(1, adapter.getChildrenCount(0));
    }

    @Test
    public void testGetChild() {
        LocationModel actualLocationModel = adapter.getChild(0,0);
        assertEquals(locationModel.getId(), actualLocationModel.getId());
        assertEquals(locationModel.getName(), actualLocationModel.getName());
    }

    @Test
    public void testGetGroupId() {
        assertEquals(0, adapter.getGroupId(0));
    }

    @Test
    public void testHasStableIds() {
        assertFalse(adapter.hasStableIds());
    }

    @Test
    public void testClearChecks() {
        assertTrue(adapter.getChild(0,0).isChecked());
        Whitebox.setInternalState(adapter, "checkedGroup" , new boolean[0]);
        adapter.clearChecks();
        assertFalse(adapter.getChild(0,0).isChecked());
    }

    @Test
    public void testIsChildSelectable() {
        assertTrue(adapter.isChildSelectable(0,0));
    }

    @Test
    public void testInitializeLocationList() {
        List<Pair<String, String>> expectedListGroup  = Collections.singletonList(Pair.create("parent-location-id2", "Chadiza"));
        LocationModel expectedLocationModel = new LocationModel();
        expectedLocationModel.setId("location-id2");
        expectedLocationModel.setName("Chadiza OA");
        expectedLocationModel.setChecked(true);
        List<LocationModel> expectedLocationModels = Collections.singletonList(expectedLocationModel);
        HashMap<String, List<LocationModel>> expectedChildLocationsMap = new HashMap<>();
        expectedChildLocationsMap.put("parent-location-id2", expectedLocationModels);

        adapter.initializeLocationList(expectedListGroup, expectedChildLocationsMap);

        LocationModel actualLocationModel = adapter.getChild(0,0);
        assertEquals(expectedLocationModel.getId(), actualLocationModel.getId());
        assertEquals(expectedLocationModel.getName(), actualLocationModel.getName());

    }

}
