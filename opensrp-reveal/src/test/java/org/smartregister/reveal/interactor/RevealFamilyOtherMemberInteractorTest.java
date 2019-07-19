package org.smartregister.reveal.interactor;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.Context;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;

import java.util.ArrayList;
import java.util.UUID;

import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class RevealFamilyOtherMemberInteractorTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private FamilyOtherMemberProfileContract.Presenter presenter;

    @Mock
    private CommonRepository commonRepository;

    @Mock
    private CommonPersonObject familyHeadPersonObject;

    private RevealFamilyOtherMemberInteractor interactor;

    @Before
    public void setUp() {
        Context.bindtypes = new ArrayList<>();
        interactor = new RevealFamilyOtherMemberInteractor();
        Whitebox.setInternalState(interactor, "commonRepository", commonRepository);
    }

    @Test
    public void testGetFamilyHead() {
        String familyHead = UUID.randomUUID().toString();
        when(commonRepository.findByBaseEntityId(familyHead)).thenReturn(familyHeadPersonObject);
        interactor.getFamilyHead(presenter, familyHead);
        verify(presenter, timeout(ASYNC_TIMEOUT)).onFetchFamilyHead(familyHeadPersonObject);

    }

}
