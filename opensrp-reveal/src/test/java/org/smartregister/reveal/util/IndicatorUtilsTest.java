package org.smartregister.reveal.util;


import net.sqlcipher.Cursor;
import net.sqlcipher.MatrixCursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.domain.Task;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.IndicatorDetails;
import org.smartregister.reveal.model.TaskDetails;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.powermock.api.mockito.PowerMockito.when;

/**
 * Created by ndegwamartin on 2019-09-27.
 */
public class IndicatorUtilsTest extends BaseUnitTest {

    private Map<String, Set<Task>> taskTestMap = new HashMap<>();
    @Mock
    private SQLiteDatabase sqLiteDatabase;

    @Before
    public void setUp() {

        TestingUtils.createTasks().forEach(t -> {
            if (taskTestMap.containsKey(t.getStructureId())) {
                taskTestMap.get(t.getStructureId()).add(t);
            } else {
                Set<Task> tasks = new HashSet<>();
                tasks.add(t);
                taskTestMap.put(t.getStructureId(), tasks);
            }
        });
    }


    @Test
    public void validateSetupForTaskTestMapCorrectly() {

        assertNotNull(taskTestMap);
        assertTrue(taskTestMap.size() > 0);


    }

    @Test
    public void testProcessTaskDetailsPopulatesTaskDetailsListCorrectly() {

        List<TaskDetails> taskDetailsList = IndicatorUtils.processTaskDetails(taskTestMap);

        assertNotNull(taskDetailsList);
        assertTrue(taskDetailsList.size() > 0);
        assertEquals(TestingUtils.createTasks().size(), taskDetailsList.size());

    }

    @Test
    public void testProcessIndicatorsReturnsCorrectIndicatorDetails() {

        List<TaskDetails> taskDetailsList = IndicatorUtils.processTaskDetails(taskTestMap);
        IndicatorDetails indicatorDetails = IndicatorUtils.processIndicators(taskDetailsList);

        assertNotNull(indicatorDetails);

        assertEquals(3, indicatorDetails.getSprayed());
        assertEquals(1, indicatorDetails.getNotSprayed());
        assertEquals(2, indicatorDetails.getNotVisited());
        assertEquals(50, indicatorDetails.getProgress());
        assertEquals(6, indicatorDetails.getTotalStructures());
        assertEquals(1, indicatorDetails.getIneligible());

    }

    @Test
    public void testPopulateSprayIndicatorsPopulatesCorrectly() {
        Country country = BuildConfig.BUILD_COUNTRY;
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.ZAMBIA);
        List<TaskDetails> taskDetailsList = IndicatorUtils.processTaskDetails(taskTestMap);
        IndicatorDetails indicatorDetails = IndicatorUtils.processIndicators(taskDetailsList);
        List<String> sprayIndicatorList = IndicatorUtils.populateSprayIndicators(RuntimeEnvironment.application, indicatorDetails);

        assertNotNull(sprayIndicatorList);

        assertEquals(14, sprayIndicatorList.size());
        assertEquals(getString(R.string.spray_coverage), sprayIndicatorList.get(0));
        assertEquals("50%", sprayIndicatorList.get(1));
        assertEquals(getString(R.string.structures_remaining_90), sprayIndicatorList.get(2));
        assertEquals("2", sprayIndicatorList.get(3));
        assertEquals(getString(R.string.total_structures), sprayIndicatorList.get(4));
        assertEquals("6", sprayIndicatorList.get(5));
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, country);

    }

    @Test
    public void testGetNamibiaIndicatorsShouldReturnCorrectIndicators() {
        String locationId = UUID.randomUUID().toString();
        String planId = UUID.randomUUID().toString();
        when(sqLiteDatabase.rawQuery(anyString(), eq(new String[]{locationId, planId}))).thenReturn(getCursor());
        IndicatorDetails indicatorDetails = IndicatorUtils.getNamibiaIndicators(locationId, planId, sqLiteDatabase);
        assertEquals(75, indicatorDetails.getFoundStructures());
        assertEquals(74, indicatorDetails.getSprayed());
        assertEquals(1, indicatorDetails.getNotSprayed());
        verify(sqLiteDatabase).rawQuery(anyString(), eq(new String[]{locationId, planId}));
    }

    public static Cursor getCursor() {
        MatrixCursor cursor = new MatrixCursor(new String[]{"foundStruct", "sprayedStruct", "notSprayedStruct"});
        cursor.addRow(new Object[]{
                75, 74, 1
        });
        return cursor;
    }
}
