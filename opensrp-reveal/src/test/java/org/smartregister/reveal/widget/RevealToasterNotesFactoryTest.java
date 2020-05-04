package org.smartregister.reveal.widget;

import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.vijay.jsonwizard.utils.ValidationStatus;
import com.vijay.jsonwizard.validators.edittext.RequiredValidator;
import com.vijay.jsonwizard.views.JsonFormFragmentView;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.smartregister.reveal.BaseUnitTest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 5/4/20.
 */

public class RevealToasterNotesFactoryTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    JsonFormFragmentView jsonFormFragmentView;

    @Mock
    ViewGroup parentView;

    @Mock
    ViewGroup grandParentView;

    @Mock
    TextView textView;

    @Before
    public void setUp() {
        when(textView.getParent()).thenReturn(parentView);
        when(parentView.getParent()).thenReturn(grandParentView);
        when(grandParentView.getVisibility()).thenReturn(View.VISIBLE);
    }

    @Test
    public void testValidateForInvalidStatus() {
        String expectedErrorMsg = "ValidationError";
        RequiredValidator requiredValidator = mock(RequiredValidator.class);
        when(requiredValidator.getErrorMessage()).thenReturn(expectedErrorMsg);
        when(textView.getTag(com.vijay.jsonwizard.R.id.v_required)).thenReturn(requiredValidator);

        ValidationStatus validationStatus = RevealToasterNotesFactory.validate(jsonFormFragmentView, textView);
        assertNotNull(validationStatus);
        assertFalse(validationStatus.isValid());
        assertEquals(expectedErrorMsg, validationStatus.getErrorMessage());
    }

    @Test
    public void testValidateForValidstatus() {
        ValidationStatus validationStatus = RevealToasterNotesFactory.validate(jsonFormFragmentView, textView);
        assertNotNull(validationStatus);
        assertTrue(validationStatus.isValid());
        assertNull(validationStatus.getErrorMessage());
    }
}
