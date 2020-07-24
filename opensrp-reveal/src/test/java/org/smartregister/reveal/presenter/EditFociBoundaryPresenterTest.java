package org.smartregister.reveal.presenter;

import android.content.Context;
import android.view.View;
import android.widget.TextView;

import androidx.appcompat.app.AlertDialog;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.shadows.ShadowAlertDialog;
import org.smartregister.domain.Location;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.EditFociboundaryContract;
import org.smartregister.reveal.util.EditBoundaryState;
import org.smartregister.reveal.util.TestingUtils;

import static android.content.DialogInterface.BUTTON_POSITIVE;
import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 7/20/20.
 */

public class EditFociBoundaryPresenterTest extends BaseUnitTest {
    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private EditFociboundaryContract.Interactor interactor;

    @Mock
    private EditFociboundaryContract.View editFociBoundaryView;

    @Mock
    private View view;

    private EditFociBoundaryPresenter presenter;

    private Context context = RuntimeEnvironment.application;

    @Before
    public void setUp() {
        presenter = new EditFociBoundaryPresenter(editFociBoundaryView);
        Whitebox.setInternalState(presenter, "interactor", interactor);
        when(editFociBoundaryView.getContext()).thenReturn(context);
    }

    @Test
    public void testOnDeletePoint() {
        presenter = spy(presenter);
        presenter.onDeletePoint(view);
        verify(presenter).displayDeletePointDialog(view);
    }

    @Test
    public void testOnCancelEditBoundaryChanges() {
        presenter = spy(presenter);
        presenter.onCancelEditBoundaryChanges();
        verify(presenter).onCancelEditBoundaryChanges();
    }

    @Test
    public void testOnSaveEditedBoundary() {
        Location location = TestingUtils.gson.fromJson(TestingUtils.operationalAreaGeoJSON, Location.class);
        presenter.onSaveEditedBoundary(location);
        verify(interactor).saveLocation(location);
    }

    @Test
    public void testOnEditedBoundarySaved() {
        presenter.onEditedBoundarySaved();
        verify(editFociBoundaryView).exitEditBoundaryActivity();
    }

    @Test
    public void testOnSavePoint() {
        presenter.onSavePoint();
        verify(editFociBoundaryView).toggleButtons(EditBoundaryState.FINISHED);
        verify(editFociBoundaryView).setToolbarTitle(R.string.edit_boundary);
    }

    @Test
    public void testOnEditPoint() {
        presenter.onEditPoint();
        verify(editFociBoundaryView).toggleButtons(EditBoundaryState.EDITTING);
        verify(editFociBoundaryView).displaySnackBar(R.string.drag_selected_point_msg);
        verify(editFociBoundaryView).setToolbarTitle(R.string.change_point);
    }

    @Test
    public void testDisplayDeletePointDialog() {
        presenter.displayDeletePointDialog(view);
        AlertDialog alertDialog = (AlertDialog) ShadowAlertDialog.getLatestDialog();
        assertTrue(alertDialog.isShowing());
        TextView msgTv = alertDialog.findViewById(android.R.id.message);
        assertEquals(getString(R.string.delete_point_msg), msgTv.getText());
        alertDialog.getButton(BUTTON_POSITIVE).callOnClick();
        verify(editFociBoundaryView).deletePoint(view);
    }

    @Test
    public void testDisplayDiscardChangesDialog() {
        presenter.displayDiscardChangesDialog();
        AlertDialog alertDialog = (AlertDialog) ShadowAlertDialog.getLatestDialog();
        assertTrue(alertDialog.isShowing());
        TextView msgTv = alertDialog.findViewById(android.R.id.message);
        assertEquals(getString(R.string.discard_changes_msg), msgTv.getText());
        alertDialog.getButton(BUTTON_POSITIVE).callOnClick();
        verify(editFociBoundaryView).exitEditBoundaryActivity();
    }

}
