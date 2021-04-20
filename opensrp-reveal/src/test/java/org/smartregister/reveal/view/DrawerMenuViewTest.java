package org.smartregister.reveal.view;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.view.View;
import android.view.ViewTreeObserver;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;
import androidx.core.view.GravityCompat;
import androidx.drawerlayout.widget.DrawerLayout;

import com.google.android.material.navigation.NavigationView;

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
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.util.Country;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 4/9/20.
 */

public class DrawerMenuViewTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private DrawerLayout mDrawerLayout;

    @Mock
    private BaseDrawerContract.DrawerActivity activity;

    @Mock
    private TextView planTextView;

    @Mock
    private TextView operationalAreaTextView;

    @Mock
    private TextView p2pSyncTextView;

    @Mock
    private BaseDrawerContract.Presenter presenter;

    @Captor
    private ArgumentCaptor<String> stringArgumentCaptor;

    private DrawerMenuView drawerMenuView;

    private Context context = RuntimeEnvironment.application;
    private AppCompatActivity mockActivity = mock(AppCompatActivity.class);
    private ProgressBar progress = new ProgressBar(context);
    private TextView progressLabel = new TextView(context);
    private TextView syncButton = new TextView(context);
    private TextView syncLabel = new TextView(context);

    @Before
    public void setUp() {
        drawerMenuView = new DrawerMenuView(activity);
    }

    @Test
    public void testDrawerMenuViewIsCreated() {
        assertNotNull(drawerMenuView);
        assertNotNull(Whitebox.getInternalState(drawerMenuView, "activity"));
        assertNotNull(Whitebox.getInternalState(drawerMenuView, "presenter"));
    }

    @Test
    public void testSetPlan() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "planTextView", planTextView);

        drawerMenuView.setPlan("FI plan");
        verify(planTextView).setText(stringArgumentCaptor.capture());
        assertEquals("FI plan", stringArgumentCaptor.getValue());
    }

    @Test
    public void testGetPlan() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "planTextView", planTextView);
        when(planTextView.getText()).thenReturn("FI plan");

        String actualPlan = drawerMenuView.getPlan();
        assertEquals("FI plan", actualPlan);
    }

    @Test
    public void testSetOperationalArea() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "operationalAreaTextView", operationalAreaTextView);

        drawerMenuView.setOperationalArea("Akros_1");
        verify(operationalAreaTextView).setText(stringArgumentCaptor.capture());
        assertEquals("Akros_1", stringArgumentCaptor.getValue());
    }


    @Test
    public void testInitializeDrawerLayoutForZambiaAndSenegal() throws PackageManager.NameNotFoundException {
        drawerMenuView = spy(drawerMenuView);
        doReturn(mockActivity).when(activity).getActivity();
        NavigationView navView = mock(NavigationView.class);
        View headView = mock(View.class);
        DrawerLayout drawerLayout = mock(DrawerLayout.class);
        PackageManager packageManager = mock(PackageManager.class);
        when(packageManager.getPackageInfo(anyString(), anyInt())).thenReturn(mock(PackageInfo.class));
        when(mockActivity.findViewById(R.id.drawer_layout)).thenReturn(drawerLayout);
        when(mockActivity.findViewById(R.id.nav_view)).thenReturn(navView);
        when(mockActivity.getPackageManager()).thenReturn(packageManager);
        when(mockActivity.getPackageName()).thenReturn("");
        when(navView.getHeaderView(0)).thenReturn(headView);
        when(headView.getViewTreeObserver()).thenReturn(mock(ViewTreeObserver.class));
        when(headView.findViewById(R.id.btn_navMenu_p2pSyncBtn)).thenReturn(p2pSyncTextView);
        when(headView.findViewById(R.id.btn_navMenu_offline_maps)).thenReturn(planTextView);
        when(headView.findViewById(R.id.sync_button)).thenReturn(planTextView);
        when(headView.findViewById(R.id.logout_button)).thenReturn(planTextView);
        when(headView.findViewById(R.id.plan_selector)).thenReturn(planTextView);
        when(headView.findViewById(R.id.operational_area_selector)).thenReturn(planTextView);
        when(headView.findViewById(R.id.application_updated)).thenReturn(planTextView);
        when(headView.findViewById(R.id.application_version)).thenReturn(planTextView);
        when(headView.findViewById(R.id.btn_navMenu_summaryForms)).thenReturn(planTextView);
        when(headView.findViewById(R.id.operator_label)).thenReturn(planTextView);
        when(headView.findViewById(R.id.facility_label)).thenReturn(planTextView);
        when(headView.findViewById(R.id.district_label)).thenReturn(planTextView);
        when(headView.findViewById(R.id.btn_navMenu_filled_forms)).thenReturn(planTextView);

        Country buildCountry = BuildConfig.BUILD_COUNTRY;
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, Country.ZAMBIA);
        drawerMenuView.initializeDrawerLayout();
        verify(p2pSyncTextView).setVisibility(View.VISIBLE);
        Whitebox.setInternalState(BuildConfig.class, BuildConfig.BUILD_COUNTRY, buildCountry);
    }

    @Test
    public void testGetOperationalArea() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "operationalAreaTextView", operationalAreaTextView);
        when(operationalAreaTextView.getText()).thenReturn("Akros_1");

        String actualOperationalArea = drawerMenuView.getOperationalArea();
        assertEquals("Akros_1", actualOperationalArea);
    }

    @Test
    public void testLockNavigationDrawerForSelection() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "mDrawerLayout", mDrawerLayout);

        drawerMenuView.lockNavigationDrawerForSelection();
        verify(mDrawerLayout).openDrawer(GravityCompat.START);
        verify(mDrawerLayout).setDrawerLockMode(DrawerLayout.LOCK_MODE_LOCKED_OPEN);
    }

    @Test
    public void testUnlockNavigationDrawer() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "mDrawerLayout", mDrawerLayout);
        when(mDrawerLayout.getDrawerLockMode(GravityCompat.START)).thenReturn(DrawerLayout.LOCK_MODE_LOCKED_OPEN);

        drawerMenuView.unlockNavigationDrawer();
        verify(mDrawerLayout).closeDrawer(GravityCompat.START);
        verify(mDrawerLayout).setDrawerLockMode(DrawerLayout.LOCK_MODE_UNLOCKED);
    }

    @Test
    public void testOpenDrawerLayout() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "mDrawerLayout", mDrawerLayout);

        drawerMenuView.openDrawerLayout();
        verify(mDrawerLayout).openDrawer(GravityCompat.START);
    }

    @Test
    public void testCloseDrawerLayout() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "mDrawerLayout", mDrawerLayout);
        Whitebox.setInternalState(drawerMenuView, "presenter", presenter);
        when(presenter.isPlanAndOperationalAreaSelected()).thenReturn(true);

        drawerMenuView.closeDrawerLayout();
        verify(mDrawerLayout).closeDrawer(GravityCompat.START);
    }

    @Test
    public void testOpAreaSelectorOnClick() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "presenter", presenter);
        View opAreaSelectoreView = new View(context);
        opAreaSelectoreView.setId(R.id.operational_area_selector);

        drawerMenuView.onClick(opAreaSelectoreView);
        verify(presenter).onShowOperationalAreaSelector();
    }

    @Test
    public void testPlanSelectorOnClick() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "presenter", presenter);
        View planSelectoreView = new View(context);
        planSelectoreView.setId(R.id.plan_selector);

        drawerMenuView.onClick(planSelectoreView);
        verify(presenter).onShowPlanSelector();
    }

    @Test
    public void testOfflineMapsButtonOnclick() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "presenter", presenter);
        View offlineMapsButton = new View(context);
        offlineMapsButton.setId(R.id.btn_navMenu_offline_maps);

        drawerMenuView.onClick(offlineMapsButton);
        verify(presenter).onShowOfflineMaps();
    }

    @Test
    public void testOnResume() {
        drawerMenuView = spy(drawerMenuView);
        Whitebox.setInternalState(drawerMenuView, "presenter", presenter);

        drawerMenuView.onResume();
        verify(presenter).onViewResumed();
    }

    @Test
    public void testToggleProgressBarViewTrue() {
        doReturn(mockActivity).when(activity).getActivity();
        doReturn(progress).when(mockActivity).findViewById(eq(R.id.sync_progress_bar));
        doReturn(progressLabel).when(mockActivity).findViewById(eq(R.id.sync_progress_bar_label));
        doReturn(syncButton).when(mockActivity).findViewById(eq(R.id.sync_button));
        doReturn(syncLabel).when(mockActivity).findViewById(eq(R.id.sync_label));

        drawerMenuView.toggleProgressBarView(true);

        assertEquals(progress.getVisibility(), View.VISIBLE);
        assertEquals(progressLabel.getVisibility(), View.VISIBLE);
        assertEquals(syncButton.getVisibility(), View.INVISIBLE);
        assertEquals(syncLabel.getVisibility(), View.INVISIBLE);
    }

    @Test
    public void testToggleProgressBarViewFalse() {
        doReturn(mockActivity).when(activity).getActivity();
        doReturn(progress).when(mockActivity).findViewById(eq(R.id.sync_progress_bar));
        doReturn(progressLabel).when(mockActivity).findViewById(eq(R.id.sync_progress_bar_label));
        doReturn(syncButton).when(mockActivity).findViewById(eq(R.id.sync_button));
        doReturn(syncLabel).when(mockActivity).findViewById(eq(R.id.sync_label));

        drawerMenuView.toggleProgressBarView(false);

        assertEquals(progress.getVisibility(), View.INVISIBLE);
        assertEquals(progressLabel.getVisibility(), View.INVISIBLE);
        assertEquals(syncButton.getVisibility(), View.VISIBLE);
        assertEquals(syncLabel.getVisibility(), View.VISIBLE);
    }

    @Test
    public void testLockNavigationDrawerForSelectionShouldDisplayAlertDialog() {
        drawerMenuView = spy(drawerMenuView);
        AppCompatActivity appCompatActivity = Robolectric.buildActivity(AppCompatActivity.class).create().get();
        when(drawerMenuView.getContext()).thenReturn(appCompatActivity);
        Whitebox.setInternalState(drawerMenuView, "mDrawerLayout", mDrawerLayout);

        drawerMenuView.lockNavigationDrawerForSelection(R.string.select_campaign_operational_area_title, R.string.select_campaign_operational_area_title);

        verify(mDrawerLayout).openDrawer(GravityCompat.START);
        verify(mDrawerLayout).setDrawerLockMode(DrawerLayout.LOCK_MODE_LOCKED_OPEN);
    }

}
