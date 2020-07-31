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
import org.smartregister.repository.BaseRepository;
import org.smartregister.repository.LocationRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.EditFociboundaryContract;
import org.smartregister.reveal.util.TestingUtils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;

/**
 * Created by Richard Kareko on 7/20/20.
 */

public class EditFociBoundaryInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private EditFociboundaryContract.Presenter presenter;

    @Mock
    private LocationRepository locationRepository;

    @Captor
    private ArgumentCaptor<Location> locationArgumentCaptor;

    private EditFociBoundaryInteractor editFociBoundaryInteractor;

    @Before
    public void setUp() {
        editFociBoundaryInteractor = new EditFociBoundaryInteractor(presenter);
        Whitebox.setInternalState(editFociBoundaryInteractor, "locationRepository", locationRepository);
    }

    @Test
    public void testSaveLocation() {
        RevealApplication.getInstance().getContext().allSharedPreferences().updateANMUserName("user1132");
        Location expectedLocation = TestingUtils.gson.fromJson(TestingUtils.operationalAreaGeoJSON, Location.class);
        assertNull(expectedLocation.getProperties().getUsername());
        assertEquals(BaseRepository.TYPE_Synced,expectedLocation.getSyncStatus());

        editFociBoundaryInteractor.saveLocation(expectedLocation);
        verify(locationRepository, timeout(ASYNC_TIMEOUT)).addOrUpdate(locationArgumentCaptor.capture());
        Location actualLocation = locationArgumentCaptor.getValue();
        assertEquals(BaseRepository.TYPE_Unsynced,actualLocation.getSyncStatus());
        assertEquals(expectedLocation.getId(), actualLocation.getId());
        assertEquals(expectedLocation.getGeometry(), actualLocation.getGeometry());
        assertEquals(expectedLocation.getType(), actualLocation.getType());
        assertEquals("user1132", actualLocation.getProperties().getUsername());
        verify(presenter, timeout(ASYNC_TIMEOUT)).onEditedBoundarySaved();
    }

}
