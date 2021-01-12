package org.smartregister.reveal.fragment;

import android.view.LayoutInflater;

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
import org.robolectric.Shadows;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.presenter.FamilyRegisterFragmentPresenter;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import static org.smartregister.Context.bindtypes;

/**
 * Created by Richard Kareko on 1/12/21.
 */

public class FamilyRegisterFragmentTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private FamilyRegisterFragmentPresenter presenter;

    private FamilyRegisterFragment fragment;

    private FragmentActivity activity;

    @Before
    public void setUp() {
        bindtypes = new ArrayList<>();
        fragment = new FamilyRegisterFragment();
        Whitebox.setInternalState(fragment, "presenter", presenter);
        activity = Robolectric.buildActivity(AppCompatActivity.class).create().start().get();
        activity.setContentView(R.layout.fragment_base_register);
        activity.getSupportFragmentManager().beginTransaction().add(fragment, "Family Registration").commit();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(fragment);
        assertNotNull(Whitebox.getInternalState(fragment, "presenter"));
    }

    @Test
    public void testGetMainCondition() {
        String expectedMainCondition = "date_removed is null";
        when(presenter.getMainCondition()).thenReturn(expectedMainCondition);
        String actualMainCondition = fragment.getMainCondition();
        assertEquals(expectedMainCondition, actualMainCondition);
    }

    @Test
    public void testGetDefaultSortQuery() {
        String expectedDefaultSortQuery = "last_interacted_with DESC ";
        when(presenter.getDefaultSortQuery()).thenReturn(expectedDefaultSortQuery);
        String actualDefaultSortQuery = fragment.getDefaultSortQuery();
        assertEquals(expectedDefaultSortQuery, actualDefaultSortQuery);
    }

    @Test
    public void testSetupViews() {
        fragment = spy(fragment);
        when(fragment.getActivity()).thenReturn(activity);
        when(fragment.getContext()).thenReturn(activity);
        android.view.View view = LayoutInflater.from(activity).inflate(R.layout.fragment_base_register, null);
        fragment.setupViews(view);
        assertEquals(R.color.white, Shadows.shadowOf(fragment.getSearchView().getBackground()).getCreatedFromResId());
        assertEquals(android.view.View.GONE, view.findViewById(R.id.filter_sort_layout).getVisibility());

    }

}
