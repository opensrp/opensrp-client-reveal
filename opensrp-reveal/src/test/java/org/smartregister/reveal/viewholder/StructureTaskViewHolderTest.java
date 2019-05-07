package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

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

}
