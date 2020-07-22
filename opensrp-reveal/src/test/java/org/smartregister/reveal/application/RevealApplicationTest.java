package org.smartregister.reveal.application;

import android.content.Context;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.family.domain.FamilyMetadata;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.util.Country;
import org.smartregister.service.UserService;

import io.ona.kujaku.data.realm.RealmDatabase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * Created by Ephraim Kigamba - ekigamba@ona.io on 2020-02-03
 */

public class RevealApplicationTest extends BaseUnitTest {

    private RevealApplication revealApplication;
    private UserService userService;
    private Context context;

    @Mock
    private AllSharedPreferences allSharedPreferences;

    @Before
    public void setUp() {

        MockitoAnnotations.initMocks(this);

        revealApplication = Mockito.spy(RevealApplication.getInstance());
        context = RuntimeEnvironment.application;
        userService = Mockito.spy(revealApplication.getContext().userService());
        ReflectionHelpers.setField(revealApplication.getContext(), "userService", userService);
    }

    @Test
    public void onTimeChangedShouldLogoutCurrentUser() {
        revealApplication.onTimeChanged();

        Mockito.verify(revealApplication, Mockito.times(1)).logoutCurrentUser();
        Mockito.verify(userService, Mockito.times(1)).forceRemoteLogin(ArgumentMatchers.anyString());
    }

    @Test
    public void onTimeZoneChangedShouldLogoutCurrentUser() {
        revealApplication.onTimeChanged();
        Mockito.verify(revealApplication, Mockito.times(1)).logoutCurrentUser();
        Mockito.verify(userService, Mockito.times(1)).forceRemoteLogin(ArgumentMatchers.anyString());
    }

    @Test
    public void testGetRealmDatabase() {
        RealmDatabase realmDatabase = revealApplication.getRealmDatabase(context);
        assertNotNull(realmDatabase);
    }

    @Test
    public void testGetFamilyMetaDataForThailand() {
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.THAILAND);
        FamilyMetadata actualMetaData = revealApplication.getMetadata();
        assertNotNull(actualMetaData);
        assertEquals("opensrp_id", actualMetaData.uniqueIdentifierKey);
        assertEquals("ec_family", actualMetaData.familyRegister.tableName);
        assertEquals("Family Registration", actualMetaData.familyRegister.registerEventType);
        assertEquals("family_register", actualMetaData.familyRegister.config);
        assertEquals("family_head", actualMetaData.familyRegister.familyHeadRelationKey);
    }

    @Test
    public void testGetPasswordRetrievesCharPasswordCorrectly() {

        ReflectionHelpers.setField(revealApplication.getContext(), "allSharedPreferences", allSharedPreferences);
        Mockito.doReturn(DUMMY_USERNAME).when(allSharedPreferences).fetchRegisteredANM();
        Mockito.doReturn(DUMMY_PASSWORD).when(userService).getGroupId(ArgumentMatchers.anyString());
        ReflectionHelpers.setField(userService, "allSharedPreferences", allSharedPreferences);

        revealApplication.setPassword(null);

        char[] password = revealApplication.getPassword();

        assertNotNull(password);
        assertEquals(DUMMY_PASSWORD, password);

        Mockito.verify(userService, Mockito.times(1)).getGroupId(DUMMY_USERNAME);

    }

}