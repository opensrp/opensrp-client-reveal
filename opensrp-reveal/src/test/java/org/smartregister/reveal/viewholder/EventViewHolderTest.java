package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.view.View;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.util.Constants.EventType;
import org.smartregister.reveal.util.TestingUtils;

import static org.junit.Assert.assertEquals;

/**
 * Created by samuelgithengi on 2/9/21.
 */
public class EventViewHolderTest extends BaseUnitTest {

    private Context context = RuntimeEnvironment.application;

    @Mock
    private View.OnClickListener registerClickListener;

    @Mock
    private View.OnClickListener paginationClickListener;

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
}
