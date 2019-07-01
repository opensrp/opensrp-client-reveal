package org.smartregister.reveal.interactor;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.BaseFormFragmentContract;

import java.util.ArrayList;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 6/18/19.
 */
public class FormFragmentInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private BaseFormFragmentContract.Presenter presenter;

    @Mock
    private CommonRepository commonRepository;

    private BaseFormFragmentInteractor interactor;


    @Before
    public void setUp() {
        org.smartregister.Context.bindtypes = new ArrayList<>();
        interactor = new BaseFormFragmentInteractor(presenter);
        Whitebox.setInternalState(interactor, "commonRepository", commonRepository);
    }

    @Test
    public void testFindNumberOfMembers() {
        when(commonRepository.countSearchIds(anyString())).thenReturn(12);
        String structureId = UUID.randomUUID().toString();
        JSONObject form = new JSONObject();
        interactor.findNumberOfMembers(structureId, form);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onFetchedMembersCount(12, form);
        verify(commonRepository, timeout(ASYNC_TIMEOUT)).countSearchIds(anyString());
    }


}
