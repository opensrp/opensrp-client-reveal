package org.smartregister.reveal.fragment;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.annotation.Config;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.presenter.EventRegisterFragmentPresenter;
import org.smartregister.reveal.shadow.DrawerMenuViewShadow;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.view.EventRegisterActivity;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * Created by Richard Kareko on 12/1/20.
 */

@Config(shadows = {DrawerMenuViewShadow.class})
public class EventRegisterFragmentTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private EventRegisterFragment fragment;

    private EventRegisterActivity activity;

    @Mock
    private RevealJsonFormUtils jsonFormUtils;

    @Mock
    private EventRegisterFragmentPresenter presenter;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        fragment = new EventRegisterFragment();
        Whitebox.setInternalState(fragment, "presenter", presenter);
        activity = Robolectric.buildActivity(EventRegisterActivity.class).create().start().get();
        activity.setContentView(R.layout.activity_base_register);
        activity.getSupportFragmentManager().beginTransaction().add(0, fragment).commit();

    }

    @Test
    public void testOnCreate() {
        assertNotNull(fragment);
    }

    @Test
    public void testGetLayout() {
        assertEquals(R.layout.fragment_event_register, fragment.getLayout());
    }

    @Test
    public void testInitializePresenter() {
        Whitebox.setInternalState(fragment, "presenter", (Object[]) null);
        assertNull(Whitebox.getInternalState(fragment, "presenter"));
        fragment.initializePresenter();
        assertNotNull(Whitebox.getInternalState(fragment, "presenter"));
    }

}
