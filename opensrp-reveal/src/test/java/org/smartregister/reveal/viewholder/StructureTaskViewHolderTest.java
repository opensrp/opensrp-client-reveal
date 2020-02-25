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
import org.robolectric.RuntimeEnvironment;
import org.robolectric.Shadows;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.TestingUtils;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.smartregister.reveal.util.Constants.Intervention.BEDNET_DISTRIBUTION;

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
    public void testSetTaskActionForCompleteBednetWithLastEditted() {
        SimpleDateFormat dateFormat = new SimpleDateFormat("M/dd", Locale.getDefault());
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
    public void testSetTaskNameAndCode() {

        viewHolder.setTaskName("Charity Otala, 30", BEDNET_DISTRIBUTION);
        TextView nameTextView = viewHolder.itemView.findViewById(R.id.task_name);
        TextView detailsTextView = viewHolder.itemView.findViewById(R.id.task_details);

        assertEquals("Charity Otala, 30", nameTextView.getText());
        assertEquals(BEDNET_DISTRIBUTION, detailsTextView.getText());
        assertEquals(View.VISIBLE, detailsTextView.getVisibility());


    }

}
