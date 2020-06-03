package org.smartregister.reveal.presenter;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.ChildRegisterFragmentContract;
import org.smartregister.reveal.model.ChildModel;
import org.smartregister.reveal.util.TestGenericInteractor;

public class ChildRegisterFragmentPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private ChildRegisterFragmentPresenter presenter;

    @Mock
    private ChildRegisterFragmentContract.View view;

    @Mock
    private ChildModel model;

    @Before
    public void setUp() {
        presenter = new ChildRegisterFragmentPresenter();
        presenter.with(view)
                .withModel(model);

        presenter.withInteractor(new TestGenericInteractor());
    }

    @Test
    public void testSearch() {
        presenter = Mockito.spy(presenter);

        presenter.search(null, null);
        Mockito.verify(presenter).fetchList(Mockito.any(), Mockito.any());
    }

    @Test
    public void testStartMDAForm() throws JSONException {
        // no error
        JSONObject jsonObject = new JSONObject();
        Mockito.doReturn(jsonObject).when(model).getMDAForm(Mockito.any(), Mockito.anyString());

        presenter.startMDAForm(RuntimeEnvironment.application, "12345");
        Mockito.verify(view).startJsonForm(Mockito.any(), Mockito.any());
        Mockito.verify(model).getMDAForm(Mockito.any(), Mockito.eq("12345"));
    }

    @Test
    public void testStartMDAFormNoForm() throws JSONException {
        Mockito.doReturn(null).when(model).getMDAForm(Mockito.any(), Mockito.anyString());
        presenter.startMDAForm(RuntimeEnvironment.application, "12345");
        Mockito.verify(view).onFetchError(Mockito.any());
    }

    @Test
    public void testStartMDAFormWithError() throws JSONException {
        Mockito.doThrow(new JSONException("Sample")).when(model).getMDAForm(Mockito.any(), Mockito.anyString());
        presenter.startMDAForm(RuntimeEnvironment.application, "12345");
        Mockito.verify(view).onFetchError(Mockito.any());
    }

    @Test
    public void testStartChildRegistrationForm() throws JSONException {
        JSONObject jsonObject = new JSONObject();
        Mockito.doReturn(jsonObject).when(model).getRegistrationForm(Mockito.any());

        presenter.startChildRegistrationForm(RuntimeEnvironment.application);
        Mockito.verify(view).startJsonForm(Mockito.any(), Mockito.any());
        Mockito.verify(model).getRegistrationForm(Mockito.any());
    }

    @Test
    public void testStartChildRegistrationFormNoForm() throws JSONException {
        Mockito.doReturn(null).when(model).getRegistrationForm(Mockito.any());
        presenter.startChildRegistrationForm(RuntimeEnvironment.application);
        Mockito.verify(view).onFetchError(Mockito.any());
    }

    @Test
    public void testStartChildRegistrationFormWithError() throws JSONException {
        Mockito.doThrow(new JSONException("Sample")).when(model).getRegistrationForm(Mockito.any());
        presenter.startChildRegistrationForm(RuntimeEnvironment.application);
        Mockito.verify(view).onFetchError(Mockito.any());
    }
}
