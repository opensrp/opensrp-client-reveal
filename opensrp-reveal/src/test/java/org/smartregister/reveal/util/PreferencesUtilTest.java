package org.smartregister.reveal.util;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.repository.AllSharedPreferences;
import org.smartregister.reveal.BaseUnitTest;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_DISTRICT;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_FACILITY;
import static org.smartregister.reveal.util.Constants.Preferences.CURRENT_PROVINCE;
import static org.smartregister.reveal.util.Constants.Preferences.FACILITY_LEVEL;

/**
 * Created by Richard Kareko on 4/15/20.
 */

public class PreferencesUtilTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private AllSharedPreferences allSharedPreferences;

    private PreferencesUtil preferencesUtil;

    @Before
    public void setUp() {
        preferencesUtil = PreferencesUtil.getInstance();
        Whitebox.setInternalState(preferencesUtil, "allSharedPreferences", allSharedPreferences);
    }

    @Test
    public void testSetCurrentFacility() {
        preferencesUtil.setCurrentFacility("Akros_1");
        verify(allSharedPreferences).savePreference(CURRENT_FACILITY, "Akros_1");
    }

    @Test
    public void testGetCurrentFacility() {
        when(allSharedPreferences.getPreference(CURRENT_FACILITY)).thenReturn("Akros_1");

        String actualCurrentFacility = preferencesUtil.getCurrentFacility();
        assertEquals("Akros_1", actualCurrentFacility);
    }

    @Test
    public void testSetCurrentDistrict() {
        preferencesUtil.setCurrentDistrict("Chadiza");
        verify(allSharedPreferences).savePreference(CURRENT_DISTRICT, "Chadiza");
    }

    @Test
    public void testGetCurrentDistrict() {
        when(allSharedPreferences.getPreference(CURRENT_DISTRICT)).thenReturn("Chadiza");

        String actualCurrentDistrict = preferencesUtil.getCurrentDistrict();
        assertEquals("Chadiza", actualCurrentDistrict);
    }

    @Test
    public void testSetCurrentProvince() {
        preferencesUtil.setCurrentProvince("Lusaka");
        verify(allSharedPreferences).savePreference(CURRENT_PROVINCE, "Lusaka");
    }

    @Test
    public void testGetCurrentProvince() {
        when(allSharedPreferences.getPreference(CURRENT_PROVINCE)).thenReturn("Lusaka");

        String actualCurrentProvince = preferencesUtil.getCurrentProvince();
        assertEquals("Lusaka", actualCurrentProvince);
    }

    @Test
    public void testGetPreferenceValue() {
        when(allSharedPreferences.getPreference(CURRENT_PROVINCE)).thenReturn("Lusaka");

        String actualPreferenceValue = preferencesUtil.getPreferenceValue(CURRENT_PROVINCE);
        assertEquals("Lusaka", actualPreferenceValue);
    }

    @Test
    public void testSetCurrentFacilityLevel() {
        preferencesUtil.setCurrentFacilityLevel("Village");
        verify(allSharedPreferences).savePreference(FACILITY_LEVEL, "Village");
    }

    @Test
    public void testGetCurrentFacilityLevel() {
        when(allSharedPreferences.getPreference(FACILITY_LEVEL)).thenReturn("Village");

        String actualFacilityLevel = preferencesUtil.getCurrentFacilityLevel();
        assertEquals("Village", actualFacilityLevel);
    }

}
