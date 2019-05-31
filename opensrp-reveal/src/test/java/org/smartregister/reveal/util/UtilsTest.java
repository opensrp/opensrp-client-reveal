package org.smartregister.reveal.util;

import com.mapbox.mapboxsdk.maps.MapboxMap;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Utils.getDrawOperationalAreaBoundaryAndLabel;
import static org.smartregister.reveal.util.Utils.getInterventionLabel;

/**
 * Created by Vincent Karuri on 08/05/2019
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({PreferencesUtil.class, MapboxMap.class, RevealApplication.class})
public class UtilsTest {

    @Test
    public void testGetInterventionLabel() throws Exception {
        PowerMockito.mockStatic(PreferencesUtil.class);
        PreferencesUtil preferencesUtil = mock(PreferencesUtil.class);
        PowerMockito.when(PreferencesUtil.class, "getInstance").thenReturn(preferencesUtil);
        when(preferencesUtil.getCurrentPlan()).thenReturn("IRS_1");
        assertEquals(getInterventionLabel(), R.string.irs);
    }

    @Test
    public void testgetDrawOperationalAreaBoundaryAndLabelReturnsDefaultValueWhenSettingsValueIsNull() throws Exception {
        RevealApplication revealApplication = initRevealApplicationMock();

        when(revealApplication.getGlobalConfigs()).thenReturn(new HashMap<>());
        assertEquals(getDrawOperationalAreaBoundaryAndLabel(), Constants.CONFIGURATION.DEFAULT_DRAW_OPERATIONAL_AREA_BOUNDARY_AND_LABEL);
    }

    @Test
    public void testgetDrawOperationalAreaBoundaryAndLabelReturnsFalseWhenSettingsValueIsFalse() throws Exception {
        RevealApplication revealApplication = initRevealApplicationMock();

        Map<String, String> settings = new HashMap<>();
        settings.put("draw_operational_area_boundary_and_label", "false");

        when(revealApplication.getGlobalConfigs()).thenReturn(settings);
        assertFalse(getDrawOperationalAreaBoundaryAndLabel());
    }

    @Test
    public void testgetDrawOperationalAreaBoundaryAndLabelReturnsTrueWhenSettingsValueIsTrue() throws Exception {
        RevealApplication revealApplication = initRevealApplicationMock();

        Map<String, String> settings = new HashMap<>();
        settings.put("draw_operational_area_boundary_and_label", "true");

        when(revealApplication.getGlobalConfigs()).thenReturn(settings);
        assert(getDrawOperationalAreaBoundaryAndLabel());
    }

    private RevealApplication initRevealApplicationMock() throws Exception {
        PowerMockito.mockStatic(RevealApplication.class);
        RevealApplication revealApplication = mock(RevealApplication.class);
        PowerMockito.when(RevealApplication.class, "getInstance").thenReturn(revealApplication);
        return revealApplication;
    }

}
