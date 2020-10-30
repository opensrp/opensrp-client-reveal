package org.smartregister.reveal.fragment;

import android.content.Context;
import android.view.LayoutInflater;
import android.widget.Button;

import androidx.appcompat.app.AppCompatActivity;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.CoreLibrary;
import org.smartregister.P2POptions;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.ExpandableListViewAdapter;
import org.smartregister.reveal.presenter.LocationPickerFragmentPresenter;

import java.util.ArrayList;

import edu.emory.mathcs.backport.java.util.Collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 10/30/20.
 */

public class LocationPickerFragmentTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private ExpandableListViewAdapter mExpandableListAdapter;

    @Mock
    private LocationPickerFragmentPresenter presenter;

    @Mock
    private P2POptions p2POptions;

    @Captor
    private ArgumentCaptor<String[]> stringArrayArgumentCaptor;

    private LocationPickerFragment fragment;

    private Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes= new ArrayList<>();
        fragment = new LocationPickerFragment();
        AppCompatActivity activity; activity = Robolectric.buildActivity(AppCompatActivity.class).create().start().get();
        activity.setContentView(R.layout.fragment_location_picker);
        activity.getSupportFragmentManager().beginTransaction().add(fragment, "P2P Sync Locations").commit();
        Whitebox.setInternalState(CoreLibrary.getInstance(), "p2POptions", p2POptions);

    }

    @Test
    public void testOnCreateView() {
        Whitebox.setInternalState(fragment, "presenter", presenter);
        fragment.onCreateView(LayoutInflater.from(context), null, null);
        assertNotNull(Whitebox.getInternalState(fragment, "btnP2PSync"));
        assertNotNull(Whitebox.getInternalState(fragment, "mExpandableListView"));
        verify(presenter).fetchAvailableLocations(null);
    }

    @Test
    public void testInitiatingP2PSync() {
        Button btnP2PSync = Whitebox.getInternalState(fragment, "btnP2PSync");
        Whitebox.setInternalState(fragment, "mExpandableListAdapter", mExpandableListAdapter);
        when(mExpandableListAdapter.getSelectedLocationIds()).thenReturn(Collections.singletonList("location-id1"));
        btnP2PSync.performClick();
        verify(p2POptions).setLocationsFilter(stringArrayArgumentCaptor.capture());
        assertNotNull(stringArrayArgumentCaptor.getValue());
        assertEquals("location-id1", stringArrayArgumentCaptor.getValue()[0]);
    }
}
