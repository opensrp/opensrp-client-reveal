package org.smartregister.reveal.interactor;

import android.content.Context;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.domain.Location;
import org.smartregister.repository.LocationRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.DownloadedOfflineMapsContract;

import java.util.Collections;
import java.util.List;

import io.ona.kujaku.data.realm.RealmDatabase;

import static org.mockito.ArgumentMatchers.anyObject;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 1/24/20.
 */

public class DownloadedOfflineMapsInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private DownloadedOfflineMapsContract.Presenter presenter;

    @Mock
    private LocationRepository locationRepository;

    @Mock
    private RealmDatabase realmDatabase;

    private Context context = RuntimeEnvironment.application;

    private DownloadedOfflineMapsInteractor interactor;

    private String locationId;

    @Before
    public void setUp() {
        interactor = new DownloadedOfflineMapsInteractor(presenter, context);
        Whitebox.setInternalState(interactor, "locationRepository", locationRepository);
        Whitebox.setInternalState(interactor, "realmDatabase", realmDatabase);
        locationId = "location_1";
    }

    @Test
    public void testFetchLocationsWithOfflineMapDownloadsWithNullParams() {
        interactor.fetchLocationsWithOfflineMapDownloads(null);
        verify(presenter).onOAsWithOfflineDownloadsFetched(null);
        verifyNoMoreInteractions(presenter);
        verifyNoMoreInteractions(locationRepository);
    }

    @Test
    public void testFetchLocationsWithOfflineMapDownloads() {

        List<String> locationIds = Collections.singletonList(locationId);
        List<Location> locations = Collections.singletonList(initLocation());

        when(locationRepository.getLocationsByIds(locationIds, false)).thenReturn(locations);
        when(realmDatabase.getTasks()).thenReturn(null);
        interactor.fetchLocationsWithOfflineMapDownloads(anyObject());
        verify(presenter).onOAsWithOfflineDownloadsFetched(null);
    }

    private Location initLocation() {
        Location location = new Location();
        location.setType("Polygon");
        location.setId(locationId);
        location.setJurisdiction(true);
        return location;
    }

}
