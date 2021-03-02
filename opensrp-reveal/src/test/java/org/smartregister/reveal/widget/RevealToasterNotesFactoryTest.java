package org.smartregister.reveal.widget;

import android.content.Context;
import android.view.View;
import android.widget.TextView;

import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interfaces.CommonListener;
import com.vijay.jsonwizard.interfaces.JsonApi;
import com.vijay.jsonwizard.utils.ValidationStatus;
import com.vijay.jsonwizard.validators.edittext.RequiredValidator;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;

import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 1/12/21.
 */
public class RevealToasterNotesFactoryTest extends BaseUnitTest {

    private RevealToasterNotesFactory factory;

    private Context context;

    @Mock
    private JsonFormFragment formFragment;
    @Mock
    private CommonListener listener;
    @Mock
    private JSONObject jsonObject;
    @Mock
    private JsonApi jsonApi;
    @Captor
    private ArgumentCaptor<TextView> textViewArgumentCaptor;


    @Before
    public void setUp() throws JSONException {
        factory = new RevealToasterNotesFactory();
        jsonObject = new JSONObject("{\"key\":\"displayRecommendedNumNetsToDistribute\",\"openmrs_entity_parent\":\"\",\"openmrs_entity\":\"\",\"openmrs_entity_id\":\"\",\"type\":\"toaster_notes\",\"text\":\"2 nets should be distributed\",\"text_color\":\"#000000\",\"toaster_type\":\"info\",\"v_required\":{\"value\":true,\"err\":\"Toast notes Field required\"}}");
        context = RuntimeEnvironment.application;
        when(formFragment.getJsonApi()).thenReturn(jsonApi);
    }

    @Test
    public void testGetViewsFromJsonShouldReturnView() throws Exception {
        List<View> views = factory.getViewsFromJson("step1", context, formFragment, jsonObject, listener);
        assertEquals(1, views.size());
        verify(jsonApi).addFormDataView(textViewArgumentCaptor.capture());
        TextView textView = textViewArgumentCaptor.getValue();
        assertEquals(com.vijay.jsonwizard.R.id.toaster_notes_text, textView.getId());
        assertEquals("step1:displayRecommendedNumNetsToDistribute", textView.getTag(com.vijay.jsonwizard.R.id.address));
        assertEquals("displayRecommendedNumNetsToDistribute", textView.getTag(com.vijay.jsonwizard.R.id.key));
        MatcherAssert.assertThat(textView.getTag(com.vijay.jsonwizard.R.id.v_required), Matchers.instanceOf(RequiredValidator.class));
    }

    @Test
    public void testValidateShouldReturnFailedValidation() throws Exception {
        factory.getViewsFromJson("step1", context, formFragment, jsonObject, listener);
        verify(jsonApi).addFormDataView(textViewArgumentCaptor.capture());
        TextView textView = textViewArgumentCaptor.getValue();
        ValidationStatus validateStatus = RevealToasterNotesFactory.validate(formFragment, textView);
        assertFalse(validateStatus.isValid());
        assertEquals("Toast notes Field required", validateStatus.getErrorMessage());
    }

    @Test
    public void testValidateShouldReturnValidValidationWhenViewIsNotShown() throws Exception {
        factory.getViewsFromJson("step1", context, formFragment, jsonObject, listener);
        verify(jsonApi).addFormDataView(textViewArgumentCaptor.capture());
        TextView textView = textViewArgumentCaptor.getValue();
        View view = (View) textView.getParent().getParent();
        view.setVisibility(View.GONE);
        ValidationStatus validateStatus = RevealToasterNotesFactory.validate(formFragment, textView);
        assertTrue(validateStatus.isValid());
        assertNull(validateStatus.getErrorMessage());
    }
}
