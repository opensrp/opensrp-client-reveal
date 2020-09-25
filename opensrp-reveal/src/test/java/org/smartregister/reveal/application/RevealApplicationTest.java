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
import org.smartregister.dto.UserAssignmentDTO;
import org.smartregister.family.domain.FamilyMetadata;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.service.UserService;

import java.util.Collections;
import java.util.UUID;

import io.ona.kujaku.data.realm.RealmDatabase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

/**
 * Created by Ephraim Kigamba - ekigamba@ona.io on 2020-02-03
 */

public class RevealApplicationTest extends BaseUnitTest {

    private RevealApplication revealApplication;
    private UserService userService;
    private Context context;

    @Mock
    private AllSharedPreferences allSharedPreferences;

    @Mock
    private PreferencesUtil preferencesUtil;

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

        verify(revealApplication, Mockito.times(1)).logoutCurrentUser();
        verify(userService, Mockito.times(1)).forceRemoteLogin(ArgumentMatchers.anyString());
    }

    @Test
    public void onTimeZoneChangedShouldLogoutCurrentUser() {
        revealApplication.onTimeChanged();
        verify(revealApplication, Mockito.times(1)).logoutCurrentUser();
        verify(userService, Mockito.times(1)).forceRemoteLogin(ArgumentMatchers.anyString());
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
        assertEquals("Family_Registration", actualMetaData.familyRegister.registerEventType);
        assertEquals("family_register", actualMetaData.familyRegister.config);
        assertEquals("family_head", actualMetaData.familyRegister.familyHeadRelationKey);
    }

    @Test
    public void testOnUserAssignmentRevokedShouldClearOAsAndPlan() {
        Whitebox.setInternalState(PreferencesUtil.class, "instance", preferencesUtil);
        String planId = UUID.randomUUID().toString();
        String operationalArea = UUID.randomUUID().toString();
        UserAssignmentDTO userAssignmentDTO = UserAssignmentDTO.builder().jurisdictions(Collections.singleton(operationalArea)).plans(Collections.singleton(planId)).build();
        when(preferencesUtil.getCurrentOperationalAreaId()).thenReturn(operationalArea);
        when(preferencesUtil.getCurrentPlanId()).thenReturn(planId);
        revealApplication.onUserAssignmentRevoked(userAssignmentDTO);
        verify(preferencesUtil).setCurrentOperationalArea(null);
        verify(preferencesUtil).setCurrentPlan(null);
        verify(preferencesUtil).setCurrentPlanId(null);
        Whitebox.setInternalState(PreferencesUtil.class, "instance", (PreferencesUtil) null);
    }

    @Test
    public void testOnUserAssignmentRevokedShouldNotClearOAsAndPlan() {
        Whitebox.setInternalState(PreferencesUtil.class, "instance", preferencesUtil);
        String planId = UUID.randomUUID().toString();
        String operationalArea = UUID.randomUUID().toString();
        UserAssignmentDTO userAssignmentDTO = UserAssignmentDTO.builder().jurisdictions(Collections.singleton(operationalArea)).plans(Collections.singleton(planId)).build();
        revealApplication.onUserAssignmentRevoked(userAssignmentDTO);
        verify(preferencesUtil).getCurrentOperationalAreaId();
        verify(preferencesUtil).getCurrentPlanId();
        verifyZeroInteractions(preferencesUtil);
        Whitebox.setInternalState(PreferencesUtil.class, "instance", (PreferencesUtil) null);
    }

}