package org.smartregister.reveal.presenter;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.domain.Location;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.LocationPickerFragmentContract;
import org.smartregister.reveal.util.TestingUtils;

import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.verify;

/**
 * Created by Richard Kareko on 10/1/20.
 */

public class LocationPickerFragmentPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private LocationPickerFragmentContract.Interactor interactor;

    @Mock
    private LocationPickerFragmentContract.View view;

    private LocationPickerFragmentPresenter presenter;

    @Captor
    private ArgumentCaptor<List<String>> stringListArgumentCaptor;

    @Captor
    private ArgumentCaptor<List<Location>> locationListArgumentCaptor;

    @Before
    public void setUp() {
        presenter = new LocationPickerFragmentPresenter(view);
        Whitebox.setInternalState(presenter, "interactor", interactor);
    }

    @Test
    public void testFetchAvailableLocations() {
        presenter.fetchAvailableLocations(Collections.singletonList("id1"));
        verify(interactor).fetchAvailableLocations(stringListArgumentCaptor.capture());
        assertEquals("id1", stringListArgumentCaptor.getValue().get(0));
    }

    @Test
    public void testOnFetchAvailableLocations() {
        Location expectedLocation = TestingUtils.getOperationalArea();
        presenter.onFetchAvailableLocations(Collections.singletonList(expectedLocation));
        verify(view).setAvailableLocations(locationListArgumentCaptor.capture());
        Location actualLocation = locationListArgumentCaptor.getValue().get(0);
        assertNotNull(actualLocation);
        assertEquals(expectedLocation.getId(), actualLocation.getId());
        assertEquals(expectedLocation.getType(), actualLocation.getType());
    }

}
