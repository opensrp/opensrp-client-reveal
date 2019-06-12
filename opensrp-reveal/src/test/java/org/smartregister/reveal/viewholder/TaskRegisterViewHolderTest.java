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
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.PreferencesUtil;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 3/26/19.
 */
public class TaskRegisterViewHolderTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private PreferencesUtil prefsUtil;

    @Mock
    private TaskDetails taskDetails;

    @Mock
    private CardDetails cardDetails;

    @Mock
    private View.OnClickListener onClickListener;

    private Context context = RuntimeEnvironment.application;

    private TaskRegisterViewHolder viewHolder;

    @Before
    public void setUp() {
        View view = LayoutInflater.from(context).inflate(R.layout.task_register_row, null);
        viewHolder = new TaskRegisterViewHolder(view);
    }

    @Test
    public void testSetIcon() {
        viewHolder.setIcon(R.drawable.ic_bcc);
        ImageView iconView = viewHolder.itemView.findViewById(R.id.task_icon);
        assertEquals(View.VISIBLE, iconView.getVisibility());
        assertEquals(R.drawable.ic_bcc, Shadows.shadowOf(iconView.getDrawable()).getCreatedFromResId());

        viewHolder.setIcon(R.drawable.ic_icon_danger);
        assertEquals(R.drawable.ic_icon_danger, Shadows.shadowOf(iconView.getDrawable()).getCreatedFromResId());
    }

    @Test
    public void testSetTaskName() {
        viewHolder.setTaskName("Structure 19");
        assertEquals("Structure 19", ((TextView) viewHolder.itemView.findViewById(R.id.task_name)).getText());
    }


    @Test
    public void testSetDistanceFromStructure() {
        viewHolder.setDistanceFromStructure(34, false);
        assertEquals("34 m away", ((TextView) viewHolder.itemView.findViewById(R.id.distance_from_structure)).getText());
    }

    @Test
    public void testSetDistanceFromCenterOfOperationalArea() {
        Whitebox.setInternalState(viewHolder, "prefsUtil", prefsUtil);
        when(prefsUtil.getCurrentOperationalArea()).thenReturn("MTI_13");
        viewHolder.setDistanceFromStructure(34, true);
        assertEquals("34 m from MTI_13 center", ((TextView) viewHolder.itemView.findViewById(R.id.distance_from_structure)).getText());
    }


    @Test
    public void testHideDistanceFromStructure() {
        viewHolder.hideDistanceFromStructure();
        assertEquals(View.GONE, viewHolder.itemView.findViewById(R.id.distance_from_structure).getVisibility());
    }

    @Test
    public void testHideIcon() {
        viewHolder.hideIcon();
        assertEquals(View.GONE, viewHolder.itemView.findViewById(R.id.task_icon).getVisibility());
    }


    @Test
    public void testSetTaskDetailsForNotSprayed() {
        viewHolder.setTaskDetails(Constants.BusinessStatus.NOT_SPRAYED, "No one Home");
        TextView taskDetails = viewHolder.itemView.findViewById(R.id.task_details);
        assertEquals(View.VISIBLE, taskDetails.getVisibility());
        assertEquals("Reason: No one Home", taskDetails.getText());
    }

    @Test
    public void testSetTaskDetails() {
        viewHolder.setTaskDetails(Constants.BusinessStatus.SPRAYED, "No one Home");
        TextView taskDetails = viewHolder.itemView.findViewById(R.id.task_details);
        assertEquals(View.GONE, taskDetails.getVisibility());
        assertEquals("", taskDetails.getText());
    }

    @Test
    public void testSetTaskActionWithoutCardDetails() {
        viewHolder.setTaskAction("Record\n Index", taskDetails, null, onClickListener);
        TextView taskDetails = viewHolder.itemView.findViewById(R.id.task_action);
        assertEquals(View.VISIBLE, taskDetails.getVisibility());
        assertEquals("Record\n Index", taskDetails.getText());
        taskDetails.performClick();
        verify(onClickListener).onClick(taskDetails);
    }

    @Test
    public void testSetTaskActionWithCardDetails() {
        when(cardDetails.getStatusColor()).thenReturn(R.color.sprayed);
        viewHolder.setTaskAction("Record\n Status", taskDetails, cardDetails, onClickListener);
        TextView taskDetails = viewHolder.itemView.findViewById(R.id.task_action);
        assertEquals(View.VISIBLE, taskDetails.getVisibility());
        assertEquals("Record\n Status", taskDetails.getText());
        //assertEquals(context.getColor(R.color.sprayed), taskDetails.getCurrentTextColor());
        taskDetails.performClick();
        verify(onClickListener).onClick(taskDetails);

    }
}
