package org.smartregister.reveal.application;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.service.UserService;

/**
 * Created by Ephraim Kigamba - ekigamba@ona.io on 2020-02-03
 */

public class RevealApplicationTest extends BaseUnitTest {

    private RevealApplication revealApplication;
    private UserService userService;

    @Before
    public void setUp() throws Exception {
        revealApplication = Mockito.spy(RevealApplication.getInstance());
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
}