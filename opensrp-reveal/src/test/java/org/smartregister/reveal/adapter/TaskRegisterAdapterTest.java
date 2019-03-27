package org.smartregister.reveal.adapter;

import android.content.Context;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.domain.Task;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.viewholder.TaskRegisterViewHolder;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Created by samuelgithengi on 3/26/19.
 */
public class TaskRegisterAdapterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private View.OnClickListener registerActionHandler;

    private Context context = RuntimeEnvironment.application;

    private TaskRegisterAdapter adapter;

    private List<TaskDetails> taskDetailsList;

    @Before
    public void setUp() {
        adapter = new TaskRegisterAdapter(context, registerActionHandler);
        taskDetailsList = new ArrayList<>();
        TaskDetails taskDetails = new TaskDetails(UUID.randomUUID().toString());
        taskDetails.setDistanceFromUser(25.5f);
        taskDetails.setTaskStatus(Task.TaskStatus.COMPLETED.name());
        taskDetails.setStructureName("Kenny House");
        taskDetails.setTaskCode(Constants.Intervention.IRS);
        taskDetails.setBusinessStatus(Constants.BusinessStatus.NOT_SPRAYABLE);
        taskDetailsList.add(taskDetails);
    }

    @Test
    public void testOnCreateViewHolder() {
        LinearLayout vg = new LinearLayout(context);
        TaskRegisterViewHolder holder = adapter.onCreateViewHolder(vg, 0);
        assertNotNull(holder);
        assertNotNull(Whitebox.getInternalState(holder, "iconView"));
        assertNotNull(Whitebox.getInternalState(holder, "nameView"));
        assertNotNull(Whitebox.getInternalState(holder, "distanceView"));
        assertNotNull(Whitebox.getInternalState(holder, "actionView"));
        assertNotNull(Whitebox.getInternalState(holder, "taskDetailsView"));
    }

    @Test
    public void testonBindViewHolder() {
        LinearLayout vg = new LinearLayout(context);
        TaskRegisterViewHolder holder = adapter.onCreateViewHolder(vg, 0);
        Whitebox.setInternalState(adapter, "taskDetails", taskDetailsList);
        adapter.onBindViewHolder(holder, 0);
        assertEquals("Kenny House", ((TextView) holder.itemView.findViewById(R.id.task_name)).getText());
        assertEquals("26 m away", ((TextView) holder.itemView.findViewById(R.id.distance_from_structure)).getText());
        assertEquals("Not\nSprayable", ((TextView) holder.itemView.findViewById(R.id.task_action)).getText());
        assertEquals(View.GONE, holder.itemView.findViewById(R.id.task_details).getVisibility());
        assertEquals(View.GONE, holder.itemView.findViewById(R.id.task_icon).getVisibility());
    }


    @Test
    public void testonBindViewHolderMosquitoPoint() {
        TaskDetails task = taskDetailsList.get(0);
        task.setTaskCode(Constants.Intervention.MOSQUITO_COLLECTION);
        LinearLayout vg = new LinearLayout(context);
        TaskRegisterViewHolder holder = adapter.onCreateViewHolder(vg, 0);
        Whitebox.setInternalState(adapter, "taskDetails", taskDetailsList);
        adapter.onBindViewHolder(holder, 0);
        assertEquals(context.getString(R.string.mosquito_collection_point), ((TextView) holder.itemView.findViewById(R.id.task_name)).getText());
        assertEquals("26 m away", ((TextView) holder.itemView.findViewById(R.id.distance_from_structure)).getText());
        assertEquals("Not\nSprayable", ((TextView) holder.itemView.findViewById(R.id.task_action)).getText());
        assertEquals(View.GONE, holder.itemView.findViewById(R.id.task_details).getVisibility());
        assertEquals(View.GONE, holder.itemView.findViewById(R.id.task_icon).getVisibility());
    }

    @Test
    public void testonBindViewHolderLarvacide() {
        TaskDetails task = taskDetailsList.get(0);
        task.setTaskCode(Constants.Intervention.LARVAL_DIPPING);
        LinearLayout vg = new LinearLayout(context);
        TaskRegisterViewHolder holder = adapter.onCreateViewHolder(vg, 0);
        Whitebox.setInternalState(adapter, "taskDetails", taskDetailsList);
        adapter.onBindViewHolder(holder, 0);
        assertEquals(context.getString(R.string.larval_breeding_site), ((TextView) holder.itemView.findViewById(R.id.task_name)).getText());
        assertEquals("26 m away", ((TextView) holder.itemView.findViewById(R.id.distance_from_structure)).getText());
        assertEquals("Not\nSprayable", ((TextView) holder.itemView.findViewById(R.id.task_action)).getText());
        assertEquals(View.GONE, holder.itemView.findViewById(R.id.task_details).getVisibility());
        assertEquals(View.GONE, holder.itemView.findViewById(R.id.task_icon).getVisibility());
    }

    @Test
    public void testonBindViewHolderForBCCShouldDisplayIconAndHideDistance() {
        TaskDetails bccTask = new TaskDetails(UUID.randomUUID().toString());
        taskDetailsList.add(bccTask);
        bccTask.setTaskCode(Constants.Intervention.BCC);
        LinearLayout vg = new LinearLayout(context);
        TaskRegisterViewHolder holder = adapter.onCreateViewHolder(vg, 0);
        Whitebox.setInternalState(adapter, "taskDetails", taskDetailsList);
        adapter.onBindViewHolder(holder, 1);

        assertEquals(context.getString(R.string.bcc), ((TextView) holder.itemView.findViewById(R.id.task_name)).getText());
        assertEquals(View.GONE, holder.itemView.findViewById(R.id.distance_from_structure).getVisibility());
        assertEquals(View.VISIBLE, holder.itemView.findViewById(R.id.task_icon).getVisibility());
    }

    @Test
    public void testonBindViewHolderForUnsprayedShouldDisplayReason() {

        TaskDetails task = taskDetailsList.get(0);
        task.setBusinessStatus(Constants.BusinessStatus.NOT_SPRAYED);
        task.setTaskDetails("Locked");
        LinearLayout vg = new LinearLayout(context);
        TaskRegisterViewHolder holder = adapter.onCreateViewHolder(vg, 0);
        Whitebox.setInternalState(adapter, "taskDetails", taskDetailsList);
        adapter.onBindViewHolder(holder, 0);

        assertEquals("Kenny House", ((TextView) holder.itemView.findViewById(R.id.task_name)).getText());
        assertEquals("26 m away", ((TextView) holder.itemView.findViewById(R.id.distance_from_structure)).getText());
        assertEquals("Not\nSprayed", ((TextView) holder.itemView.findViewById(R.id.task_action)).getText());
        assertEquals("Reason: Locked", ((TextView) holder.itemView.findViewById(R.id.task_details)).getText());
        assertEquals(View.VISIBLE, holder.itemView.findViewById(R.id.task_details).getVisibility());
        assertEquals(View.GONE, holder.itemView.findViewById(R.id.task_icon).getVisibility());

    }

    @Test
    public void testGetItemCount() {
        Whitebox.setInternalState(adapter, "taskDetails", taskDetailsList);
        assertEquals(1, adapter.getItemCount());

        TaskDetails bccTask = new TaskDetails(UUID.randomUUID().toString());
        taskDetailsList.add(bccTask);

        assertEquals(2, adapter.getItemCount());
    }

    @Test
    public void testSetTaskDetails() {

        adapter = spy(adapter);
        assertEquals(0, adapter.getItemCount());
        adapter.setTaskDetails(taskDetailsList);
        assertEquals(1, adapter.getItemCount());

        TaskDetails bccTask = new TaskDetails(UUID.randomUUID().toString());
        taskDetailsList.add(bccTask);

        assertEquals(2, adapter.getItemCount());

        verify(adapter).notifyDataSetChanged();


        adapter.setTaskDetails(taskDetailsList);
        assertEquals(2, adapter.getItemCount());
        verify(adapter, times(2)).notifyDataSetChanged();
    }


}
