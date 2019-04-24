package org.smartregister.reveal.fragment;

import android.app.ProgressDialog;
import android.content.Context;
import android.support.v7.app.AppCompatActivity;
import android.view.LayoutInflater;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.shadows.ShadowToast;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.util.PreferencesUtil;

import java.util.UUID;

import static org.hamcrest.core.IsEqual.equalTo;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * Created by samuelgithengi on 4/24/19.
 */
public class StructureTasksFragmentTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private StructureTasksContract.Presenter presenter;

    @Mock
    private ProgressDialog progressDialog;

    private StructureTasksFragment fragment;

    private Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        fragment = new StructureTasksFragment();
        fragment = spy(fragment);
        doNothing().when(fragment).initDependencies();
        AppCompatActivity activity = Robolectric.buildActivity(AppCompatActivity.class).create().get();
        activity.setContentView(R.layout.activity_family_profile);
        activity.getSupportFragmentManager().beginTransaction().add(fragment, "Tasks").commit();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(Whitebox.getInternalState(fragment, "tabLayout"));
        verify(fragment).initDependencies();
    }

    @Test
    public void testOnCreateView() {
        PreferencesUtil.getInstance().setCurrentCampaignId("IRS_2019_S1");
        fragment.onCreateView(LayoutInflater.from(context), null, null);
        assertNotNull(Whitebox.getInternalState(fragment, "taskRecyclerView"));
        assertNotNull(Whitebox.getInternalState(fragment, "progressDialog"));
        assertNotNull(Whitebox.getInternalState(fragment, "adapter"));
    }

    @Test
    public void testSetStructure() {
        String id = UUID.randomUUID().toString();
        Whitebox.setInternalState(fragment, "presenter", presenter);
        fragment.setStructure(id);
        verify(presenter).findTasks(id);
    }

    @Test
    public void testDisplayToast() {
        fragment.displayToast("message");
        assertThat(ShadowToast.getTextOfLatestToast(), equalTo("message"));
    }


    @Test
    public void testShowProgressDialog() {
        Whitebox.setInternalState(fragment, "progressDialog", progressDialog);
        fragment.showProgressDialog(R.string.saving_title, R.string.saving_message);
        verify(progressDialog).show();
        verify(progressDialog).setTitle(R.string.saving_title);
        verify(progressDialog).setMessage(context.getString(R.string.saving_message));

    }


    @Test
    public void testHideProgressDialog() {
        Whitebox.setInternalState(fragment, "progressDialog", progressDialog);
        fragment.hideProgressDialog();
        verify(progressDialog).dismiss();

    }
}
