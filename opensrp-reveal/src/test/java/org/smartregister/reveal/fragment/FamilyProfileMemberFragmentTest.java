package org.smartregister.reveal.fragment;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;

import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.FragmentActivity;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.cursoradapter.RecyclerViewPaginatedAdapter;
import org.smartregister.family.fragment.BaseFamilyProfileMemberFragment;
import org.smartregister.family.presenter.BaseFamilyProfileMemberPresenter;
import org.smartregister.family.util.Constants;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;

/**
 * Created by Richard Kareko on 2/9/21.
 */

public class FamilyProfileMemberFragmentTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private BaseFamilyProfileMemberPresenter presenter;

    @Captor
    private ArgumentCaptor<Intent> intentArgumentCaptor;

    private FamilyProfileMemberFragment fragment;

    @Before
    public void setUp(){
        org.smartregister.Context.bindtypes = new ArrayList<>();
        fragment = new FamilyProfileMemberFragment();
        Whitebox.setInternalState(fragment, "presenter", presenter);
        FragmentActivity activity = Robolectric.buildActivity(AppCompatActivity.class).create().start().get();
        activity.setContentView(R.layout.fragment_base_register);
        activity.getSupportFragmentManager().beginTransaction().add(fragment, "RESIDENTS").commit();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(fragment);
        assertNotNull(Whitebox.getInternalState(fragment, "presenter"));
    }

    @Test
    public void testInitializepresenter() {
        fragment = spy(fragment);
        Whitebox.setInternalState(fragment, "presenter", (Object[]) null);
        assertNull(Whitebox.getInternalState(fragment, "presenter"));
        Bundle bundle = new Bundle();
        bundle.putString(Constants.INTENT_KEY.FAMILY_BASE_ENTITY_ID, "family-base-entity-id");
        bundle.putString(Constants.INTENT_KEY.FAMILY_HEAD, "John Doe");
        bundle.putString(Constants.INTENT_KEY.PRIMARY_CAREGIVER, "Jane");
        when(fragment.getArguments()).thenReturn(bundle);
        fragment.initializePresenter();
        assertNotNull(Whitebox.getInternalState(fragment, "presenter"));
    }

    @Test
    public void testinitializeAdapter() {
        fragment.initializeAdapter(new HashSet<>(), "John", "Jane");
        RecyclerViewPaginatedAdapter clientAdapter = Whitebox.getInternalState(fragment, "clientAdapter");
        assertNotNull(clientAdapter);
        assertEquals(20, clientAdapter.getCurrentlimit());
    }

    @Test
    public void testGoToOtherMemberProfileActivity() {

        String caseId = "case-id-1";
        String structureId = "structure-id-1";
        Bundle bundle = new Bundle();
        fragment = spy(fragment);
        when(fragment.getArguments()).thenReturn(bundle);
        CommonPersonObjectClient patient = new CommonPersonObjectClient(caseId, new HashMap<>(), "John");
        Whitebox.setInternalState(fragment, "structureId", structureId);

        fragment.goToOtherMemberProfileActivity(patient);

        verify(fragment).startActivity(intentArgumentCaptor.capture());
        assertEquals(caseId, intentArgumentCaptor.getValue().getStringExtra(Constants.INTENT_KEY.BASE_ENTITY_ID));
        assertEquals(structureId, intentArgumentCaptor.getValue().getStringExtra(STRUCTURE_ID));
    }

    @Test
    public void testOnViewClicked() {
        fragment = spy(fragment);
        when(fragment.getArguments()).thenReturn(new Bundle());
        View view = new View(RuntimeEnvironment.application);
        view.setId(R.id.patient_column);
        CommonPersonObjectClient patient = new CommonPersonObjectClient("caseId", new HashMap<>(), "John");
        view.setTag(patient);
        view.setTag(R.id.VIEW_ID, BaseFamilyProfileMemberFragment.CLICK_VIEW_NORMAL);
        fragment.onViewClicked(view);
        verify(fragment).goToOtherMemberProfileActivity(patient);
    }

    @Test
    public void testSetStructure() {
        assertNull(Whitebox.getInternalState(fragment, "structureId"));
        fragment.setStructure("id1");
        assertEquals("id1", Whitebox.getInternalState(fragment, "structureId"));
    }

}
