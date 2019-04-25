package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.view.View;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.util.DBConstants.KEY;
import org.smartregister.family.util.Utils;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;

import java.util.HashMap;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.smartregister.family.fragment.BaseFamilyProfileMemberFragment.CLICK_VIEW_NORMAL;

/**
 * Created by samuelgithengi on 4/24/19.
 */
public class FamilyMemberViewHolderTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private View.OnClickListener registerActionHandler;

    @Mock
    private View.OnClickListener paginationClickListener;

    @Captor
    private ArgumentCaptor<View> viewArgumentCaptor;

    private Context context = RuntimeEnvironment.application;

    private FamilyMemberViewHolder viewHolder;

    private CommonPersonObjectClient smartRegisterClient;

    @Mock
    private FamilyMemberViewHolder.RegisterViewHolder registerViewHolder;

    @Before
    public void setUp() {
        viewHolder = new FamilyMemberViewHolder(context, registerActionHandler, paginationClickListener);
        HashMap<String, String> map = new HashMap<>();
        map.put(KEY.FIRST_NAME, "Charity");
        map.put(KEY.LAST_NAME, "Otala");
        map.put(KEY.DOB, "1982-01-01T03:00:00.000+03:00");
        map.put(KEY.GENDER, "Female");
        smartRegisterClient = new CommonPersonObjectClient(UUID.randomUUID().toString(), null, null);
        smartRegisterClient.setColumnmaps(map);
    }

    @Test
    public void testGetView() {
        registerViewHolder = viewHolder.createViewHolder(null);
        viewHolder.getView(null, smartRegisterClient, registerViewHolder);
        String age = org.smartregister.reveal.util.Utils.getAge("1982-01-01T03:00:00.000+03:00");
        assertEquals("Charity Otala, " + age, registerViewHolder.patientNameAge.getText());
        assertEquals("Female", registerViewHolder.gender.getText());
    }

    @Test
    public void testGetViewForPatientsWithDod() {
        registerViewHolder = viewHolder.createViewHolder(null);
        smartRegisterClient.getColumnmaps().put(KEY.DOD, "2017-08-19T00:00:00.000+03:00");
        viewHolder.getView(null, smartRegisterClient, registerViewHolder);
        String dobString = Utils.getDuration(smartRegisterClient.getColumnmaps().get(KEY.DOB), smartRegisterClient.getColumnmaps().get(KEY.DOD));
        dobString = dobString.contains("y") ? dobString.substring(0, dobString.indexOf("y")) : dobString;
        assertEquals("Charity Otala, " + dobString + " (deceased)", registerViewHolder.patientNameAge.getText());
        assertEquals("Female", registerViewHolder.gender.getText());
    }

    @Test
    public void testAttachPatientOnclickListener() {
        registerViewHolder = viewHolder.createViewHolder(null);
        viewHolder.getView(null, smartRegisterClient, registerViewHolder);
        registerViewHolder.patientColumn.performClick();
        verify(registerActionHandler).onClick(viewArgumentCaptor.capture());
        assertEquals(smartRegisterClient, viewArgumentCaptor.getValue().getTag());
        assertEquals(CLICK_VIEW_NORMAL, viewArgumentCaptor.getValue().getTag(R.id.VIEW_ID));
    }


    @Test
    public void testGetCreateViewHolder() {
        registerViewHolder = viewHolder.createViewHolder(null);
        assertNotNull(registerViewHolder);
        assertNotNull(registerViewHolder.patientNameAge);
        assertNotNull(registerViewHolder.patientColumn);
    }

    @Test
    public void testCreateFooterHolder() {
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


}
