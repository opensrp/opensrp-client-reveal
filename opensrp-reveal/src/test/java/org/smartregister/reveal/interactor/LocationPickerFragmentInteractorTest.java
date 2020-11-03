package org.smartregister.reveal.interactor;

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
import org.smartregister.repository.LocationRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.LocationPickerFragmentContract;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.TestingUtils;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.Executors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 10/1/20.
 */

public class LocationPickerFragmentInteractorTest extends BaseUnitTest {
    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private LocationRepository locationRepository;

    @Mock
    private LocationPickerFragmentContract.Presenter presenter;

    private LocationPickerFragmentInteractor interactor;

    @Captor
    private ArgumentCaptor<List<String>>  stringListArgumentCaptor;

    @Captor
    private ArgumentCaptor<List<Location>>  locationListArgumentCaptor;

    @Captor
    private ArgumentCaptor<Boolean>  booleanArgumentCaptor;

    @Before
    public void setUp() {
        AppExecutors appExecutors = new AppExecutors(Executors.newSingleThreadExecutor(),
                Executors.newSingleThreadExecutor(), Executors.newSingleThreadExecutor());
        interactor = new LocationPickerFragmentInteractor(presenter);
        Whitebox.setInternalState(interactor, "appExecutors", appExecutors);
        Whitebox.setInternalState(interactor, "locationRepository", locationRepository);
    }

    @Test
    public void testFetchAvailableLocations() {
        Location expectedLocation = TestingUtils.getOperationalArea();
        when(locationRepository.getLocationsByIds(any(), anyBoolean())).thenReturn(Collections.singletonList(expectedLocation));
        interactor.fetchAvailableLocations(Collections.singletonList("id1"));

        verify(locationRepository, timeout(ASYNC_TIMEOUT)).getLocationsByIds(stringListArgumentCaptor.capture(), booleanArgumentCaptor.capture());
        assertEquals("id1", stringListArgumentCaptor.getValue().get(0));
        assertFalse(booleanArgumentCaptor.getValue());

        verify(presenter, timeout(ASYNC_TIMEOUT)).onFetchAvailableLocations(locationListArgumentCaptor.capture());
        Location actualLocation = locationListArgumentCaptor.getValue().get(0);

        assertNotNull(actualLocation);
        assertEquals(expectedLocation.getId(), actualLocation.getId());
        assertEquals(expectedLocation.getType(), actualLocation.getType());
    }
}
