package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.view.View;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.EventRegisterDetails;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.util.Constants.EventType;
import org.smartregister.reveal.util.TestingUtils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

/**
 * Created by samuelgithengi on 2/9/21.
 */
public class EventViewHolderTest extends BaseUnitTest {

    private Context context = RuntimeEnvironment.application;

    @Mock
    private View.OnClickListener registerClickListener;

    @Mock
    private View.OnClickListener paginationClickListener;

    @Captor
    private ArgumentCaptor<View> viewArgumentCaptor;

    private EventViewHolder viewHolder;

    private CommonPersonObjectClient smartRegisterClient;

    @Mock
    private EventViewHolder.RegisterViewHolder registerViewHolder;

    @Before
    public void setUp() {
        viewHolder = new EventViewHolder(context, registerClickListener, paginationClickListener);
        smartRegisterClient = TestingUtils.getCommonPersonObjectClientForEventRegister();
    }

    @Test
    public void testGetViewShouldPopulateViewHolder() {
        registerViewHolder = viewHolder.createViewHolder(null);
        viewHolder.getView(null, smartRegisterClient, registerViewHolder);
        assertEquals("04-2-2021", registerViewHolder.eventDateTextView.getText());
        assertEquals("HH Form", registerViewHolder.eventTypeTextView.getText());
        assertEquals("John Doe", registerViewHolder.sopTextView.getText());
        assertEquals("Hs 1233", registerViewHolder.householdTextView.getText());
        assertEquals("Sprayed", registerViewHolder.statusTextView.getText());
    }

    @Test
    public void testGetViewShouldPopulateCorrectStatusForDailySummaryEvent() {
        smartRegisterClient.getColumnmaps().put(DatabaseKeys.EVENT_TYPE, EventType.DAILY_SUMMARY_EVENT);
        smartRegisterClient.getColumnmaps().put(DatabaseKeys.FOUND, "15");
        smartRegisterClient.getColumnmaps().put(DatabaseKeys.SPRAYED, "12");
        registerViewHolder = viewHolder.createViewHolder(null);
        viewHolder.getView(null, smartRegisterClient, registerViewHolder);
        assertEquals("04-2-2021", registerViewHolder.eventDateTextView.getText());
        assertEquals("Daily Summary", registerViewHolder.eventTypeTextView.getText());
        assertEquals("John Doe", registerViewHolder.sopTextView.getText());
        assertEquals("Hs 1233", registerViewHolder.householdTextView.getText());
        assertEquals("15F/12S/3NS", registerViewHolder.statusTextView.getText());
    }

    @Test
    public void testGetViewShouldPopulateCorrectStatusForOtherEvents() {
        smartRegisterClient.getColumnmaps().put(DatabaseKeys.EVENT_TYPE, EventType.IRS_SA_DECISION_EVENT);
        smartRegisterClient.getColumnmaps().put(DatabaseKeys.STATUS, "Task Completed");
        registerViewHolder = viewHolder.createViewHolder(null);
        viewHolder.getView(null, smartRegisterClient, registerViewHolder);
        assertEquals("04-2-2021", registerViewHolder.eventDateTextView.getText());
        assertEquals("Irs Sa Decision", registerViewHolder.eventTypeTextView.getText());
        assertEquals("John Doe", registerViewHolder.sopTextView.getText());
        assertEquals("Hs 1233", registerViewHolder.householdTextView.getText());
        assertEquals("Task Completed", registerViewHolder.statusTextView.getText());
    }

    @Test
    public void testAttachOnclickListenerShouldPassEventTypeAndFormSubmissionId() {
        registerViewHolder = viewHolder.createViewHolder(null);
        viewHolder.getView(null, smartRegisterClient, registerViewHolder);
        registerViewHolder.itemView.performClick();
        verify(registerClickListener).onClick(viewArgumentCaptor.capture());
        EventRegisterDetails eventRegisterDetails = (EventRegisterDetails) viewArgumentCaptor.getValue().getTag(R.id.patient_column);
        assertNotNull(eventRegisterDetails);
        assertEquals(smartRegisterClient.getColumnmaps().get(DatabaseKeys.FORM_SUBMISSION_ID), eventRegisterDetails.getFormSubmissionId());
        assertEquals(smartRegisterClient.getColumnmaps().get(DatabaseKeys.EVENT_TYPE), eventRegisterDetails.getEventType());
    }

    @Test
    public void testGetCreateViewHolderShouldInitializeViews() {
        registerViewHolder = viewHolder.createViewHolder(null);
        assertNotNull(registerViewHolder);
        assertNotNull(registerViewHolder.eventDateTextView);
        assertNotNull(registerViewHolder.eventTypeTextView);
        assertNotNull(registerViewHolder.sopTextView);
        assertNotNull(registerViewHolder.householdTextView);
        assertNotNull(registerViewHolder.statusTextView);
        assertNotNull(registerViewHolder.dataCollectionDateTextView);
    }

    @Test
    public void testCreateFooterHolderShouldInitializeFooterView() {
        FooterViewHolder footer = (FooterViewHolder) viewHolder.createFooterHolder(null);
        assertNotNull(footer);
        assertNotNull(footer.nextPageView);
        assertNotNull(footer.previousPageView);
        assertNotNull(footer.pageInfoView);

    }

    @Test
    public void testUpdateClients() {
        assertNull(viewHolder.updateClients(null, null, null, null));
    }

    @Test
    public void testGetFooterView() {
        FooterViewHolder footerViewHolder = (FooterViewHolder) viewHolder.createFooterHolder(null);
        viewHolder.getFooterView(footerViewHolder, 1, 1, false, false);
        assertEquals("Page 1 of 1", footerViewHolder.pageInfoView.getText());
        assertEquals(View.INVISIBLE, footerViewHolder.nextPageView.getVisibility());
        assertEquals(View.INVISIBLE, footerViewHolder.previousPageView.getVisibility());

    }

    @Test
    public void testGetFooterViewWithNextAndPreviousPages() {
        FooterViewHolder footerViewHolder = (FooterViewHolder) viewHolder.createFooterHolder(null);
        viewHolder.getFooterView(footerViewHolder, 2, 3, true, true);
        assertEquals("Page 2 of 3", footerViewHolder.pageInfoView.getText());
        assertEquals(View.VISIBLE, footerViewHolder.nextPageView.getVisibility());
        assertEquals(View.VISIBLE, footerViewHolder.previousPageView.getVisibility());

        footerViewHolder.nextPageView.performClick();
        footerViewHolder.previousPageView.performClick();

        verify(paginationClickListener, times(2)).onClick(viewArgumentCaptor.capture());

    }

    @Test
    public void onServiceModeSelectedDoesNothing() {
        viewHolder = spy(viewHolder);
        viewHolder.onServiceModeSelected(null);
        verify(viewHolder).onServiceModeSelected(null);
        verifyNoMoreInteractions(viewHolder);
    }

    @Test
    public void testNewFormLauncher() {
        assertNull(viewHolder.newFormLauncher(null, null, null));
    }

    @Test
    public void testInflater() {
        assertNotNull(viewHolder.inflater());
    }

    @Test
    public void testIsFooterViewHolder() {
        assertTrue(viewHolder.isFooterViewHolder(viewHolder.createFooterHolder(null)));
        assertFalse(viewHolder.isFooterViewHolder(viewHolder.createViewHolder(null)));
    }

    @Test
    public void testGetViewShouldPopulateDateCollectionDateForCDDSupervision(){
        smartRegisterClient.getColumnmaps().put(DatabaseKeys.DATA_COLLECTION_DATE,"12-03-2021");
        smartRegisterClient.getColumnmaps().put(DatabaseKeys.EVENT_TYPE,EventType.CDD_SUPERVISOR_DAILY_SUMMARY);
        registerViewHolder = viewHolder.createViewHolder(null);
        viewHolder.getView(null,smartRegisterClient,registerViewHolder);
        assertEquals("12-03-2021",registerViewHolder.dataCollectionDateTextView.getText());
    }
}
