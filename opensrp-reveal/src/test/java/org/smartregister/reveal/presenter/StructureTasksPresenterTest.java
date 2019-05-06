package org.smartregister.reveal.presenter;

import android.content.Context;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.domain.Task;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.TestingUtils;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 4/18/19.
 */
public class StructureTasksPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private StructureTasksContract.View view;

    @Mock
    private StructureTasksContract.Interactor interactor;

    @Mock
    private PreferencesUtil prefsUtil;

    private Context context=RuntimeEnvironment.application;

    private StructureTasksPresenter presenter;


    @Before
    public void setUp() {
        presenter = new StructureTasksPresenter(view,context, interactor, prefsUtil);
    }

    @Test
    public void testFindTasks() {
        String campaignId = UUID.randomUUID().toString();
        String structureId = UUID.randomUUID().toString();
        when(prefsUtil.getCurrentCampaignId()).thenReturn(campaignId);
        presenter.findTasks(structureId);
        verify(interactor).findTasks(structureId, campaignId);
        verify(prefsUtil).getCurrentCampaignId();
    }


    @Test
    public void testOnTasksFound() {
        List<StructureTaskDetails> taskDetailsList = Collections.singletonList(TestingUtils.getStructureTaskDetails());
        presenter.onTasksFound(taskDetailsList);
        verify(view).setTaskDetailsList(taskDetailsList);
    }

    @Test
    public void testOnTaskSelectedFetchesStructureDetails() {
        StructureTaskDetails task = TestingUtils.getStructureTaskDetails();
        presenter.onTaskSelected(task);
        verify(interactor).getStructure(task);
        verify(view).showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
    }


    @Test
    public void testOnTaskSelectedCompletedTaskDisplayMessage() {
        StructureTaskDetails task = TestingUtils.getStructureTaskDetails();
        task.setTaskStatus(Task.TaskStatus.COMPLETED.name());
        presenter.onTaskSelected(task);
        verify(interactor, never()).getStructure(task);
        verify(view, never()).showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
        verify(view).displayToast(anyString());
    }

    @Test
    public void testOnTaskSelectedBCCTaskDisplayMessage() {
        StructureTaskDetails task = TestingUtils.getStructureTaskDetails();
        task.setTaskCode(Intervention.BCC);
        presenter.onTaskSelected(task);
        verify(interactor, never()).getStructure(task);
        verify(view, never()).showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
        verify(view).displayToast(anyString());
    }


    @Test
    public void testGetView() {
        assertEquals(view, presenter.getView());
    }


    @Test
    public void testSaveJsonForm() {
        String form = "{\"id\":\"dgfdfsfssd\"};";
        presenter.saveJsonForm(form);
        verify(interactor).saveJsonForm(form);
        verify(view).showProgressDialog(R.string.saving_title, R.string.saving_message);
    }

    @Test
    public void testOnFormSaved() {
        String structure = "234324323";
        String taskId = UUID.randomUUID().toString();
        presenter.onFormSaved(structure, taskId, Task.TaskStatus.COMPLETED, Constants.BusinessStatus.NOT_SPRAYED, Intervention.IRS);
        verify(view).hideProgressDialog();
        verify(view).updateTask(taskId, Task.TaskStatus.COMPLETED, Constants.BusinessStatus.NOT_SPRAYED);
    }


    @Test
    public void testOnStructureAdded() {
        verifyNoMoreInteractions(interactor);
        verifyNoMoreInteractions(view);
    }

    @Test
    public void testOnFormSaveFailure() {
        presenter.onFormSaveFailure(null);
        verify(view).hideProgressDialog();

    }

}
