package org.smartregister.reveal.presenter;

import org.json.JSONException;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.domain.Task;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.OtherFormsContract;
import org.smartregister.reveal.util.Constants;

import static org.mockito.Mockito.verify;

/**
 * Created by Richard Kareko on 4/9/20.
 */

public class OtherFormsPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private OtherFormsContract.Interactor interactor;

    @Mock
    private OtherFormsContract.View view;

    private OtherFormsPresenter presenter;

    @Before
    public void setUp() {
        presenter = new OtherFormsPresenter(view);
        Whitebox.setInternalState(presenter, "interactor", interactor);
    }

    @Test
    public void testSaveJsonForm() throws JSONException {
        String jsonFormString = "{\"id\" : \"test json\"}";
        presenter.saveJsonForm(jsonFormString);

        verify(view).showProgressDialog(R.string.saving_dialog_title);
        verify(interactor).saveJsonForm(jsonFormString);
    }

    @Test
    public void testOnFormSavedFailure() {
        presenter.onFormSaveFailure("summarry_forms");

        verify(view).hideProgressDialog();
    }

    @Test
    public void testOnFormSaved() {
        presenter.onFormSaved("structure_1", "task_1", Task.TaskStatus.DRAFT,
                "READY", Constants.InterventionType.FAMILY);

        verify(view).hideProgressDialog();
    }

}
