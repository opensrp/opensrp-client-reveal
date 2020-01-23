package org.smartregister.reveal.view;

import android.content.Intent;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageButton;

import com.mapbox.geojson.Feature;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.Context;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.util.Constants.INTENT_KEY;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.presenter.ListTaskPresenter;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.RevealMapHelper;
import org.smartregister.reveal.util.TestingUtils;

import java.util.ArrayList;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.robolectric.Shadows.shadowOf;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FIRST_NAME;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;

/**
 * Created by samuelgithengi on 1/23/20.
 */
public class ListTasksActivityTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private ListTasksActivity listTasksActivity;


    private android.content.Context context = RuntimeEnvironment.application;

    private ImageButton myLocationButton = new ImageButton(context);

    private ImageButton layerSwitcherFab = new ImageButton(context);

    @Mock
    private ListTaskPresenter listTaskPresenter;

    @Mock
    private RevealMapHelper revealMapHelper;

    @Mock
    private BaseDrawerContract.View drawerView;

    @Mock
    private Feature feature;

    @Before
    public void setUp() {
        Context.bindtypes = new ArrayList<>();
        listTasksActivity = Robolectric.buildActivity(ListTasksActivity.class).create().get();
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        params.topMargin = 30;
        myLocationButton.setLayoutParams(params);
        Whitebox.setInternalState(listTasksActivity, "myLocationButton", myLocationButton);
    }


    @Test
    public void testOnCreate() {
        assertNotNull(Robolectric.buildActivity(ListTasksActivity.class).create().get());
    }


    @Test
    public void testCloseSprayCardView() {
        View sprayCardView = listTasksActivity.findViewById(R.id.spray_card_view);
        sprayCardView.setVisibility(VISIBLE);
        listTasksActivity.closeCardView(R.id.btn_collapse_spray_card_view);
        assertEquals(GONE, sprayCardView.getVisibility());
    }

    @Test
    public void testCloseMosquitoCardView() {
        View cardView = listTasksActivity.findViewById(R.id.mosquito_collection_card_view);
        cardView.setVisibility(VISIBLE);
        listTasksActivity.closeCardView(R.id.btn_collapse_mosquito_collection_card_view);
        assertEquals(GONE, cardView.getVisibility());
    }


    @Test
    public void testCloseCardLarvalCardView() {
        View cardView = listTasksActivity.findViewById(R.id.larval_breeding_card_view);
        cardView.setVisibility(VISIBLE);
        listTasksActivity.closeCardView(R.id.btn_collapse_larval_breeding_card_view);
        assertEquals(GONE, cardView.getVisibility());
    }

    @Test
    public void testClosePAOTCardView() {
        View cardView = listTasksActivity.findViewById(R.id.potential_area_of_transmission_card_view);
        cardView.setVisibility(VISIBLE);
        listTasksActivity.closeCardView(R.id.btn_collapse_paot_card_view);
        assertEquals(GONE, cardView.getVisibility());
    }

    @Test
    public void testCloseIndicatorsCardView() {
        View cardView = listTasksActivity.findViewById(R.id.indicators_card_view);
        cardView.setVisibility(VISIBLE);
        listTasksActivity.closeCardView(R.id.btn_collapse_indicators_card_view);
        assertEquals(GONE, cardView.getVisibility());
    }

    @Test
    public void testCloseVerificationCardView() {
        View cardView = listTasksActivity.findViewById(R.id.irs_verification_card_view);
        cardView.setVisibility(VISIBLE);
        listTasksActivity.closeCardView(R.id.btn_collapse_irs_verification_card_view);
        assertEquals(GONE, cardView.getVisibility());
    }


    @Test
    public void testCloseAllCardViews() {
        listTasksActivity.findViewById(R.id.spray_card_view).setVisibility(VISIBLE);
        listTasksActivity.findViewById(R.id.mosquito_collection_card_view).setVisibility(VISIBLE);
        listTasksActivity.findViewById(R.id.larval_breeding_card_view).setVisibility(VISIBLE);
        listTasksActivity.findViewById(R.id.potential_area_of_transmission_card_view).setVisibility(VISIBLE);
        listTasksActivity.findViewById(R.id.indicators_card_view).setVisibility(VISIBLE);
        listTasksActivity.findViewById(R.id.irs_verification_card_view).setVisibility(VISIBLE);
        listTasksActivity.closeAllCardViews();
        assertEquals(GONE, listTasksActivity.findViewById(R.id.spray_card_view).getVisibility());
        assertEquals(GONE, listTasksActivity.findViewById(R.id.mosquito_collection_card_view).getVisibility());
        assertEquals(GONE, listTasksActivity.findViewById(R.id.larval_breeding_card_view).getVisibility());
        assertEquals(GONE, listTasksActivity.findViewById(R.id.potential_area_of_transmission_card_view).getVisibility());
        assertEquals(GONE, listTasksActivity.findViewById(R.id.indicators_card_view).getVisibility());
        assertEquals(GONE, listTasksActivity.findViewById(R.id.irs_verification_card_view).getVisibility());
    }

    @Test
    public void testPositionMyLocation() {
        listTasksActivity.positionMyLocationAndLayerSwitcher();
        FrameLayout.LayoutParams layoutParams = (FrameLayout.LayoutParams) myLocationButton.getLayoutParams();
        assertEquals(0, layoutParams.topMargin);
        assertEquals(30, layoutParams.bottomMargin);
        assertEquals(Gravity.BOTTOM | Gravity.END, layoutParams.gravity);
    }

    @Test
    public void testPositionMyLocationZambia() {
        listTasksActivity = spy(listTasksActivity);
        when(listTasksActivity.getBuildCountry()).thenReturn(Country.ZAMBIA);
        Whitebox.setInternalState(listTasksActivity, "myLocationButton", myLocationButton);
        listTasksActivity.positionMyLocationAndLayerSwitcher();
        FrameLayout.LayoutParams layoutParams = (FrameLayout.LayoutParams) myLocationButton.getLayoutParams();
        assertEquals(0, layoutParams.topMargin);
        int progressHeight = context.getResources().getDimensionPixelSize(R.dimen.progress_height);
        assertEquals(progressHeight + 40, layoutParams.bottomMargin);
        assertEquals(Gravity.BOTTOM | Gravity.END, layoutParams.gravity);
    }


    @Test
    public void testPositionLayerSwitcher() {
        listTasksActivity = spy(listTasksActivity);
        when(listTasksActivity.getBuildCountry()).thenReturn(Country.ZAMBIA);
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        layerSwitcherFab.setLayoutParams(params);
        Whitebox.setInternalState(listTasksActivity, "layerSwitcherFab", layerSwitcherFab);
        listTasksActivity.positionMyLocationAndLayerSwitcher();
        FrameLayout.LayoutParams layoutParams = (FrameLayout.LayoutParams) layerSwitcherFab.getLayoutParams();
        assertEquals(0, layoutParams.topMargin);
        int progressHeight = context.getResources().getDimensionPixelSize(R.dimen.progress_height);
        assertEquals(progressHeight + 80, layoutParams.bottomMargin);
        assertEquals(myLocationButton.getMeasuredHeight(), layoutParams.height);
        assertEquals(myLocationButton.getMeasuredWidth(), layoutParams.width);
    }

    @Test
    public void testOpenFilterTaskActivity() {
        listTasksActivity.findViewById(R.id.filter_tasks_fab).performClick();
        Intent startedIntent = shadowOf(listTasksActivity).getNextStartedActivity();
        assertEquals(FilterTasksActivity.class, shadowOf(startedIntent).getIntentClass());
    }


    @Test
    public void testOpenTaskRegister() {
        listTasksActivity.findViewById(R.id.task_register).performClick();
        Intent startedIntent = shadowOf(listTasksActivity).getNextStartedActivity();
        assertEquals(TaskRegisterActivity.class, shadowOf(startedIntent).getIntentClass());
    }


    @Test
    public void testOnAddStructure() {
        Whitebox.setInternalState(listTasksActivity, "listTaskPresenter", listTaskPresenter);
        Whitebox.setInternalState(listTasksActivity, "revealMapHelper", revealMapHelper);
        listTasksActivity.findViewById(R.id.btn_add_structure).performClick();
        verify(listTaskPresenter).onAddStructureClicked(false);
        verify(revealMapHelper).isMyLocationComponentActive(any(), any());
    }

    @Test
    public void testOnChangeSprayStatus() {
        Whitebox.setInternalState(listTasksActivity, "listTaskPresenter", listTaskPresenter);
        listTasksActivity.findViewById(R.id.change_spray_status).performClick();
        verify(listTaskPresenter).onChangeInterventionStatus(Intervention.IRS);
    }


    @Test
    public void testRecordMosquitoCollection() {
        Whitebox.setInternalState(listTasksActivity, "listTaskPresenter", listTaskPresenter);
        listTasksActivity.findViewById(R.id.btn_record_mosquito_collection).performClick();
        verify(listTaskPresenter).onChangeInterventionStatus(Intervention.MOSQUITO_COLLECTION);
    }


    @Test
    public void testRecordLarvalDipping() {
        Whitebox.setInternalState(listTasksActivity, "listTaskPresenter", listTaskPresenter);
        listTasksActivity.findViewById(R.id.btn_record_larval_dipping).performClick();
        verify(listTaskPresenter).onChangeInterventionStatus(Intervention.LARVAL_DIPPING);
    }

    @Test
    public void testEditPAOTDetails() {
        Whitebox.setInternalState(listTasksActivity, "listTaskPresenter", listTaskPresenter);
        listTasksActivity.findViewById(R.id.btn_edit_paot_details).performClick();
        verify(listTaskPresenter).onChangeInterventionStatus(Intervention.PAOT);
    }

    @Test
    public void testCloseCardView() {

        listTasksActivity.findViewById(R.id.btn_collapse_spray_card_view).performClick();
        assertEquals(GONE, listTasksActivity.findViewById(R.id.spray_card_view).getVisibility());
    }

    @Test
    public void testOtherCardView() {
        listTasksActivity.findViewById(R.id.btn_collapse_mosquito_collection_card_view).performClick();
        assertEquals(GONE, listTasksActivity.findViewById(R.id.mosquito_collection_card_view).getVisibility());
    }

    @Test
    public void testOpenDrawerMenu() {
        Whitebox.setInternalState(listTasksActivity, "drawerView", drawerView);
        listTasksActivity.findViewById(R.id.drawerMenu).performClick();
        verify(drawerView).openDrawerLayout();
    }

    @Test
    public void testOpenIndicators() {
        assertEquals(GONE, listTasksActivity.findViewById(R.id.indicators_card_view).getVisibility());
        listTasksActivity.findViewById(R.id.progressIndicatorsGroupView).performClick();
        assertEquals(VISIBLE, listTasksActivity.findViewById(R.id.indicators_card_view).getVisibility());
    }

    @Test
    public void testRegisterFamily() {
        Whitebox.setInternalState(listTasksActivity, "listTaskPresenter", listTaskPresenter);
        when(listTaskPresenter.getSelectedFeature()).thenReturn(feature);
        listTasksActivity.findViewById(R.id.register_family).performClick();
        Intent startedIntent = shadowOf(listTasksActivity).getNextStartedActivity();
        assertEquals(FamilyRegisterActivity.class, shadowOf(startedIntent).getIntentClass());
        assertEquals(GONE, listTasksActivity.findViewById(R.id.spray_card_view).getVisibility());
        assertTrue(startedIntent.hasExtra(TASK_IDENTIFIER));
    }


    @Test
    public void testOpenStructureProfile() {
        Whitebox.setInternalState(listTasksActivity, "listTaskPresenter", listTaskPresenter);
        when(listTaskPresenter.getSelectedFeature()).thenReturn(feature);
        CommonPersonObjectClient client = TestingUtils.getCommonPersonObjectClient();
        listTasksActivity.openStructureProfile(client);
        Intent startedIntent = shadowOf(listTasksActivity).getNextStartedActivity();
        assertEquals(FamilyProfileActivity.class, shadowOf(startedIntent).getIntentClass());
        assertEquals(client.getColumnmaps().get(FIRST_NAME), startedIntent.getStringExtra(INTENT_KEY.FAMILY_NAME));
        assertEquals(client.getCaseId(), startedIntent.getStringExtra(INTENT_KEY.FAMILY_BASE_ENTITY_ID));
        assertTrue(startedIntent.hasExtra(TASK_IDENTIFIER));

    }


}
