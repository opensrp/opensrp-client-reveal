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
import org.smartregister.reveal.contract.AvailableOfflineMapsContract;
import org.smartregister.reveal.model.OfflineMapModel;

import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 1/24/20.
 */

public class AvailableOfflineMapsInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private AvailableOfflineMapsContract.Presenter presenter;

    @Mock
    private LocationRepository locationRepository;

    @Captor
    private ArgumentCaptor<List<String>> locationIdsCaptor;

    @Captor
    private ArgumentCaptor<List<OfflineMapModel>> offlineMapModelListArgumentCaptor;

    @Captor
    private ArgumentCaptor<Boolean> booleanArgumentCaptor;

    private AvailableOfflineMapsInteractor interactor;

    private String locationId;

    @Before
    public void setUp() {
        interactor = new AvailableOfflineMapsInteractor(presenter);
        Whitebox.setInternalState(interactor, "locationRepository", locationRepository);
        locationId = "location_1";
    }

    @Test
    public void testFetchAvailableOAsForMapDownload() {
        List<String> locationIds = Collections.singletonList(locationId);
        List<Location> locations = Collections.singletonList(initLocation());

        when(locationRepository.getLocationsByIds(locationIds, false)).thenReturn(locations);

        interactor.fetchAvailableOAsForMapDownLoad(locationIds);
        verify(locationRepository).getLocationsByIds(locationIdsCaptor.capture(), booleanArgumentCaptor.capture());
        verify(presenter, timeout(ASYNC_TIMEOUT)).onFetchAvailableOAsForMapDownLoad(offlineMapModelListArgumentCaptor.capture());
        verifyNoMoreInteractions(presenter);
        verifyNoMoreInteractions(locationRepository);

        assertFalse(booleanArgumentCaptor.getValue().booleanValue());
        assertNotNull(offlineMapModelListArgumentCaptor.getValue());
        assertNotNull(offlineMapModelListArgumentCaptor.getValue().get(0));
        assertNotNull(offlineMapModelListArgumentCaptor.getValue().get(0).getLocation());

        Location location = offlineMapModelListArgumentCaptor.getValue().get(0).getLocation();
        assertEquals(locationId, location.getId());
        assertEquals("Polygon", location.getType());
        assertEquals(locationId, location.getId());

    }

    @Test
    public void testPopulateOfflineMapModelLost() {
        List<Location> locations = Collections.singletonList(initLocation());

        List<OfflineMapModel> offlineMapModels = interactor.populateOfflineMapModelList(locations);
        assertNotNull(offlineMapModels);
        assertNotNull(offlineMapModels.get(0));
        assertNotNull(offlineMapModels.get(0).getLocation());

        Location location = offlineMapModels.get(0).getLocation();
        assertEquals(locationId, location.getId());
        assertEquals("Polygon", location.getType());
        assertEquals(locationId, location.getId());
    }

    private Location initLocation() {
        Location location = new Location();
        location.setType("Polygon");
        location.setId(locationId);
        location.setJurisdiction(true);
        return location;
    }

}
