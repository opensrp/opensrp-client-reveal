package org.smartregister.reveal.view;

import androidx.viewpager.widget.ViewPager;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.robolectric.Robolectric;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.reveal.BaseUnitTest;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.verify;

/**
 * Created by Richard Kareko on 10/30/20.
 */

public class LocationPickerActivityTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private ViewPager viewPager;

    @Captor
    private ArgumentCaptor<ViewPagerAdapter> viewPagerAdapterArgumentCaptor;

    private LocationPickerActivity locationPickerActivity;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        locationPickerActivity = Robolectric.buildActivity(LocationPickerActivity.class).create().resume().get();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(locationPickerActivity);
    }

    @Test
    public void testSetUpViewPager() {
        assertNull(viewPager.getAdapter());
        viewPager = locationPickerActivity.setupViewPager(viewPager);
        verify(viewPager).setAdapter(viewPagerAdapterArgumentCaptor.capture());
        assertNotNull(viewPagerAdapterArgumentCaptor.getValue());
        assertEquals(1, viewPagerAdapterArgumentCaptor.getValue().getCount());
        assertEquals("P2P SYNC LOCATIONS", viewPagerAdapterArgumentCaptor.getValue().getPageTitle(0));
    }
}
