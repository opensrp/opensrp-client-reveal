package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.Shadows;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.TestingUtils;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Random;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;

/**
 * Created by samuelgithengi on 4/18/19.
 */
public class StructureTaskViewHolderTest extends BaseUnitTest {


    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private View.OnClickListener registerActionHandler;

    private StructureTaskViewHolder viewHolder;

    private Context context = RuntimeEnvironment.application;
    private final Random rand = new Random();

    @Before
    public void setUp() {
        View view = LayoutInflater.from(context).inflate(R.layout.structure_task_row, null);
        viewHolder = new StructureTaskViewHolder(view);
    }

    @Test
    public void testSetTaskName() {
        viewHolder.setTaskName("Charity Otala, 30");
        assertEquals("Charity Otala, 30", ((TextView) viewHolder.itemView.findViewById(R.id.task_name)).getText());
    }

    @Test
    public void testSetTaskNameForUnVisitedTask() {
        StructureTaskDetails taskDetails = TestingUtils.getStructureTaskDetails();
        viewHolder.setTaskAction(taskDetails, registerActionHandler);
        TextView action = viewHolder.itemView.findViewById(R.id.task_action);
        assertEquals("Register Family", action.getText());
        assertEquals(taskDetails, action.getTag(R.id.task_details));
        assertEquals(R.drawable.structure_task_action_bg, Shadows.shadowOf(action.getBackground()).getCreatedFromResId());
        assertEquals(context.getColor(R.color.task_not_done), action.getCurrentTextColor());
    }

    @Test
    public void testSetTaskNameForVisitedTask() {
        StructureTaskDetails taskDetails = TestingUtils.getStructureTaskDetails();
        taskDetails.setBusinessStatus(BusinessStatus.COMPLETE);
        viewHolder.setTaskAction(taskDetails, registerActionHandler);
        TextView action = viewHolder.itemView.findViewById(R.id.task_action);
        assertEquals(BusinessStatus.COMPLETE, action.getText());
        assertEquals(taskDetails, action.getTag(R.id.task_details));
        assertNull(action.getBackground());
        assertEquals(context.getColor(R.color.sprayed), action.getCurrentTextColor());
    }

    @Test
    public void testSetTaskActionForThailandCompleteBednetWithLastEditted() {
        List<Country> givenList = Arrays.asList(Country.THAILAND, Country.THAILAND_EN);
        Country thailand = givenList.get(rand.nextInt(givenList.size()));
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, thailand);
        final SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yy", Locale.getDefault());
        Date expectedDate = new Date();
        StructureTaskDetails taskDetails = TestingUtils.getStructureTaskDetails();
        taskDetails.setBusinessStatus(BusinessStatus.COMPLETE);
        taskDetails.setTaskCode(BEDNET_DISTRIBUTION);
        taskDetails.setLastEdited(expectedDate);
        viewHolder.setTaskAction(taskDetails, registerActionHandler);

        viewHolder.itemView.findViewById(R.id.task_name);
        ImageView viewEditImageView = viewHolder.itemView.findViewById(R.id.view_edit);
        TextView lastEditedTextView = viewHolder.itemView.findViewById(R.id.last_edited);
        ImageView viewUndoImageView = viewHolder.itemView.findViewById(R.id.view_undo);

        assertEquals(View.VISIBLE, viewEditImageView.getVisibility());
        assertEquals(View.VISIBLE, viewUndoImageView.getVisibility());
        assertEquals(View.VISIBLE, lastEditedTextView.getVisibility());

        assertEquals(context.getString(R.string.last_edited, dateFormat.format(expectedDate)), lastEditedTextView.getText());
    }

    @Test
    public void testSetTaskActionForCompleteBednetWithLastEditted() {
        List<Country> givenList = Arrays.stream(Country.values())
                .filter(country -> country != Country.THAILAND && country != Country.THAILAND_EN)
                .collect(Collectors.toList());
        Country other = givenList.get(rand.nextInt(givenList.size()));
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, other);
        Date expectedDate = new Date();
        StructureTaskDetails taskDetails = TestingUtils.getStructureTaskDetails();
        taskDetails.setBusinessStatus(BusinessStatus.COMPLETE);
        taskDetails.setTaskCode(BEDNET_DISTRIBUTION);
        taskDetails.setLastEdited(expectedDate);
        viewHolder.setTaskAction(taskDetails, registerActionHandler);

        viewHolder.itemView.findViewById(R.id.task_name);
        ImageView viewEditImageView = viewHolder.itemView.findViewById(R.id.view_edit);
        TextView lastEditedTextView = viewHolder.itemView.findViewById(R.id.last_edited);
        ImageView viewUndoImageView = viewHolder.itemView.findViewById(R.id.view_undo);

        assertEquals(View.VISIBLE, viewEditImageView.getVisibility());
        assertEquals(View.VISIBLE, viewUndoImageView.getVisibility());
        assertEquals(View.VISIBLE, lastEditedTextView.getVisibility());

        final SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd", Locale.getDefault());
        assertEquals(context.getString(R.string.last_edited, dateFormat.format(expectedDate)), lastEditedTextView.getText());
    }

    @Test
    public void testSetTaskNameAndCode() {

        viewHolder.setTaskName("Charity Otala, 30", BEDNET_DISTRIBUTION);
        TextView nameTextView = viewHolder.itemView.findViewById(R.id.task_name);
        TextView detailsTextView = viewHolder.itemView.findViewById(R.id.task_details);

        assertEquals("Charity Otala, 30", nameTextView.getText());
        assertEquals(BEDNET_DISTRIBUTION, detailsTextView.getText());
        assertEquals(View.VISIBLE, detailsTextView.getVisibility());
    }

    @Test
    public void testSetTaskNameForTestedBloodScreeningTask() {
        StructureTaskDetails taskDetails = TestingUtils.getStructureTaskDetails();
        taskDetails.setBusinessStatus(BusinessStatus.COMPLETE);
        taskDetails.setPersonTested(getString(R.string.yes));
        taskDetails.setTaskCode(BLOOD_SCREENING);
        viewHolder.setTaskAction(taskDetails, registerActionHandler);
        TextView action = viewHolder.itemView.findViewById(R.id.task_action);
        assertEquals(getString(R.string.tested), action.getText());
        assertEquals(taskDetails, action.getTag(R.id.task_details));
        assertNull(action.getBackground());
        assertEquals(context.getColor(R.color.sprayed), action.getCurrentTextColor());
    }

    @Test
    public void testSetTaskNameForNotTestedBloodScreeningTask() {
        StructureTaskDetails taskDetails = TestingUtils.getStructureTaskDetails();
        taskDetails.setBusinessStatus(BusinessStatus.COMPLETE);
        taskDetails.setPersonTested(getString(R.string.no));
        taskDetails.setTaskCode(BLOOD_SCREENING);
        viewHolder.setTaskAction(taskDetails, registerActionHandler);
        TextView action = viewHolder.itemView.findViewById(R.id.task_action);
        assertEquals(getString(R.string.not_tested), action.getText());
        assertEquals(taskDetails, action.getTag(R.id.task_details));
        assertNull(action.getBackground());
        assertEquals(context.getColor(R.color.sprayed), action.getCurrentTextColor());
    }

    @Test
    public void testSetTaskNameForVisitedCaseConfirmationTask() {
        StructureTaskDetails taskDetails = TestingUtils.getStructureTaskDetails();
        taskDetails.setTaskCode(CASE_CONFIRMATION);
        taskDetails.setBusinessStatus(COMPLETE);
        viewHolder.setTaskAction(taskDetails, registerActionHandler);
        TextView action = viewHolder.itemView.findViewById(R.id.task_action);
        assertEquals(context.getString(R.string.index_case_confirmed), action.getText());
        assertEquals(taskDetails, action.getTag(R.id.task_details));
        assertNull(action.getBackground());
        assertEquals(context.getColor(R.color.sprayed), action.getCurrentTextColor());
    }

}
