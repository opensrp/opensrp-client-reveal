package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.view.View;

import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.FragmentActivity;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.presenter.BaseFamilyOtherMemberProfileFragmentPresenter;
import org.smartregister.family.util.Constants;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;

import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 2/9/21.
 */

public class FamilyOtherMemberProfileFragmentTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private BaseFamilyOtherMemberProfileFragmentPresenter presenter;

    private FamilyOtherMemberProfileFragment fragment;

    @Before
    public void setUp(){
        org.smartregister.Context.bindtypes = new ArrayList<>();
        fragment = new FamilyOtherMemberProfileFragment();
        Whitebox.setInternalState(fragment, "presenter", presenter);
        FragmentActivity activity = Robolectric.buildActivity(AppCompatActivity.class).create().start().get();
        activity.setContentView(R.layout.fragment_base_register);
        activity.getSupportFragmentManager().beginTransaction().add(fragment, "").commit();
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
        when(fragment.getArguments()).thenReturn(bundle);
        fragment.initializePresenter();
        assertNotNull(Whitebox.getInternalState(fragment, "presenter"));
    }

    @Test
    public void testOnViewClicked() {
        fragment = spy(fragment);
        when(fragment.getArguments()).thenReturn(new Bundle());
        View view = new View(RuntimeEnvironment.application);
        view.setId(R.id.patient_column);
        CommonPersonObjectClient patient = new CommonPersonObjectClient("caseId", new HashMap<>(), "John");
        view.setTag(patient);
        fragment.onViewClicked(view);
        verify(fragment, times(2)).getActivity();
    }
}
