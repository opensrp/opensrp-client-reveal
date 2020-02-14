package org.smartregister.reveal.application;

import android.content.Context;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.service.UserService;

import io.ona.kujaku.data.realm.RealmDatabase;

import static org.junit.Assert.assertNotNull;

/**
 * Created by Ephraim Kigamba - ekigamba@ona.io on 2020-02-03
 */

public class RevealApplicationTest extends BaseUnitTest {

    private RevealApplication revealApplication;
    private UserService userService;
    private Context context;

    @Before
    public void setUp() throws Exception {
        revealApplication = Mockito.spy(RevealApplication.getInstance());
        context = RuntimeEnvironment.application;
        userService = Mockito.spy(revealApplication.getContext().userService());
        ReflectionHelpers.setField(revealApplication.getContext(), "userService", userService);
    }

    @Test
    public void onTimeChangedShouldLogoutCurrentUser() {
        revealApplication.onTimeChanged();

        Mockito.verify(revealApplication, Mockito.times(1)).logoutCurrentUser();
        Mockito.verify(userService, Mockito.times(1)).forceRemoteLogin();
    }

    @Test
    public void onTimeZoneChangedShouldLogoutCurrentUser() {
        revealApplication.onTimeChanged();
        Mockito.verify(revealApplication, Mockito.times(1)).logoutCurrentUser();
        Mockito.verify(userService, Mockito.times(1)).forceRemoteLogin();
    }

    @Test
    public void testGetRealmDatabase() {
        RealmDatabase realmDatabase = revealApplication.getRealmDatabase(context);
        assertNotNull(realmDatabase);
    }
}