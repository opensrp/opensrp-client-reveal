package org.smartregister.reveal.interactor;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.domain.Event;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.EventRegisterContract;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.TestingUtils;

import java.util.concurrent.Executors;

import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by Richard Kareko on 1/5/21.
 */

public class EventRegisterFragmentInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private EventRegisterContract.Presenter presenter;

    @Mock
    private EventClientRepository eventClientRepository;

    private EventRegisterFragmentInteractor interactor;

    @Before
    public void setUp() {
        interactor = new EventRegisterFragmentInteractor(presenter);
        AppExecutors appExecutors = new AppExecutors(Executors.newSingleThreadExecutor(),
                Executors.newSingleThreadExecutor(), Executors.newSingleThreadExecutor());
        Whitebox.setInternalState(interactor, "appExecutors", appExecutors);
        Whitebox.setInternalState(interactor, "eventClientRepository", eventClientRepository);
    }

    @Test
    public void testFindEvent() throws JSONException {
        String formSubmissionId = "form-submission-id";
        JSONObject bloodScreeningEventJson = new JSONObject(TestingUtils.bloodScreeningEventJSON);
        Event bloodScreeningEvent = RevealApplication.getInstance().getContext().getEventClientRepository().convert(TestingUtils.bloodScreeningEventJSON, Event.class);
        when(eventClientRepository.getEventsByFormSubmissionId(formSubmissionId)).thenReturn(bloodScreeningEventJson);
        when(eventClientRepository.convert(TestingUtils.bloodScreeningEventJSON, Event.class)).thenReturn(bloodScreeningEvent);

        interactor.findEvent(formSubmissionId);

        verify(eventClientRepository, timeout(ASYNC_TIMEOUT)).getEventsByFormSubmissionId(formSubmissionId);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onEventFound(bloodScreeningEvent);
    }
}
