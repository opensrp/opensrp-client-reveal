package org.smartregister.reveal.view;

import android.app.Activity;
import android.content.Intent;
import android.view.View;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.EventRegisterActivityContract;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.view.fragment.BaseRegisterFragment;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.JSON_FORM_PARAM_JSON;
import static org.smartregister.reveal.util.Constants.RequestCode.REQUEST_CODE_GET_JSON;

/**
 * Created by Richard Kareko on 8/6/20.
 */

public class EventRegisterActivityTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private RevealJsonFormUtils jsonFormUtils;

    @Mock
    private EventRegisterActivityContract.Presenter presenter;

    @Mock
    private BaseRegisterFragment mBaseFragment;

    private EventRegisterActivity eventRegisterActivity;

    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        eventRegisterActivity = Robolectric.buildActivity(EventRegisterActivity.class).create().resume().get();
    }

    @Test
    public void testOnCreate() {
        assertNotNull(Robolectric.buildActivity(EventRegisterActivity.class).create().get());
    }

    @Test
    public void testInititializePresenter() {
        eventRegisterActivity.initializePresenter();
        assertNotNull(Whitebox.getInternalState(eventRegisterActivity, "presenter"));
    }

    @Test
    public void testGetRegisterFragment() {
        BaseRegisterFragment fragment = eventRegisterActivity.getRegisterFragment();
        assertNotNull(fragment);
    }

    @Test
    public void testOnActivityResultExtended() {
        eventRegisterActivity = spy(eventRegisterActivity);
        when(eventRegisterActivity.getPresenter()).thenReturn(presenter);
        Intent intent = new Intent();
        intent.putExtra(JSON_FORM_PARAM_JSON, "{formjson}");
        eventRegisterActivity.onActivityResultExtended(REQUEST_CODE_GET_JSON, Activity.RESULT_OK, intent);
        verify(presenter).saveJsonForm("{formjson}");
    }

    @Test
    public void testOnActivityResultExtendedShouldInvokeRegisterFragment() {
        eventRegisterActivity = spy(eventRegisterActivity);
        Whitebox.setInternalState(eventRegisterActivity, "mBaseFragment", mBaseFragment);
        Intent intent = new Intent();
        eventRegisterActivity.onActivityResultExtended(REQUEST_CODE_GET_JSON, Activity.RESULT_OK, intent);
        verify(mBaseFragment).onActivityResult(REQUEST_CODE_GET_JSON, Activity.RESULT_OK, intent);
    }

    @Test
    public void testStartFormActivity() {
        JSONObject jsonObject = new JSONObject();
        Whitebox.setInternalState(eventRegisterActivity, "jsonFormUtils", jsonFormUtils);
        eventRegisterActivity.startFormActivity(jsonObject);
        verify(jsonFormUtils).startJsonForm(jsonObject, eventRegisterActivity);
    }

    @Test
    public void testGetViewIdentifiers() {
        assertEquals(1, eventRegisterActivity.getViewIdentifiers().size());
        assertEquals(Constants.EventsRegister.VIEW_IDENTIFIER, eventRegisterActivity.getViewIdentifiers().get(0));
    }

    @Test
    public void testRegisterBottomNavigationHidesBottonNav() {
        assertEquals(View.GONE, eventRegisterActivity.findViewById(R.id.bottom_navigation).getVisibility());
    }

}
