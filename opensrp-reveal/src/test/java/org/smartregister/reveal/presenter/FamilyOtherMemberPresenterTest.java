package org.smartregister.reveal.presenter;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.contract.FamilyOtherMemberContract;
import org.smartregister.family.contract.FamilyProfileContract;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.interactor.FamilyOtherMemberProfileInteractor;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.util.FamilyJsonFormUtils;
import org.smartregister.reveal.util.TestingUtils;

import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 6/3/19.
 */
public class FamilyOtherMemberPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private FamilyOtherMemberProfileContract.View view;

    @Mock
    private FamilyJsonFormUtils familyJsonFormUtils;

    @Mock
    private FamilyOtherMemberContract.Model model;

    @Mock
    private FamilyOtherMemberProfileInteractor interactor;

    @Mock
    private FamilyProfileContract.Interactor profileInteractor;

    @Mock
    private FamilyProfileContract.Model profileModel;

    private FamilyOtherMemberPresenter otherMemberPresenter;

    private CommonPersonObjectClient client = TestingUtils.getCommonPersonObjectClient();

    private String familyBaseEntityId = UUID.randomUUID().toString();

    private String baseEntityId = UUID.randomUUID().toString();

    private String familyHead = UUID.randomUUID().toString();

    private String primaryCaregiver = UUID.randomUUID().toString();

    private String villageTown = UUID.randomUUID().toString();

    private String familyName = UUID.randomUUID().toString();

    @Before
    public void setUp() {
        otherMemberPresenter = new FamilyOtherMemberPresenter(view, model, null, familyBaseEntityId, baseEntityId, familyHead, primaryCaregiver, villageTown, familyName);
        Whitebox.setInternalState(otherMemberPresenter, "familyJsonFormUtils", familyJsonFormUtils);
        Whitebox.setInternalState(otherMemberPresenter, "interactor", interactor);
    }

    @Test
    public void testStartFormForEdit() {
        otherMemberPresenter.startFormForEdit(client);
        verify(familyJsonFormUtils).getAutoPopulatedJsonEditMemberFormString(R.string.edit_member_form_title,
                RevealApplication.getInstance().getMetadata().familyMemberRegister.formName,
                client, RevealApplication.getInstance().getMetadata().familyMemberRegister.updateEventType, familyName, isFamilyHead);
        verify(view).startFormActivity(any());
    }


    @Test
    public void testRefreshProfileTopSectionCallSuper() {
        assertNull(Whitebox.getInternalState(otherMemberPresenter, "client"));
        when(view.getString(R.string.id_with_value)).thenReturn("ID: %1$s");
        otherMemberPresenter.refreshProfileTopSection(client);
        assertEquals(client, Whitebox.getInternalState(otherMemberPresenter, "client"));
    }


    @Test
    public void testOnRegistrationSaved() {
        otherMemberPresenter.onRegistrationSaved(false);
        verify(view, never()).hideProgressDialog();
        verify(view, never()).refreshList();
        verify(interactor, never()).refreshProfileView(baseEntityId, otherMemberPresenter);
    }

    @Test
    public void testOnRegistrationEdited() {
        otherMemberPresenter.onRegistrationSaved(true);
        verify(view).hideProgressDialog();
        verify(view).refreshList();
        verify(interactor).refreshProfileView(baseEntityId, otherMemberPresenter);
    }

    @Test
    public void testUpdateFamilyMember() {
        Whitebox.setInternalState(otherMemberPresenter, "profileInteractor", profileInteractor);
        Whitebox.setInternalState(otherMemberPresenter, "profileModel", profileModel);
        String json = "{}";
        FamilyEventClient eventClient = new FamilyEventClient(null, null);
        when(profileModel.processUpdateMemberRegistration(json, familyBaseEntityId)).thenReturn(eventClient);
        otherMemberPresenter.updateFamilyMember(json);
        verify(view).showProgressDialog(R.string.saving_dialog_title);
        verify(profileModel).processUpdateMemberRegistration(json, familyBaseEntityId);
        verify(profileInteractor).saveRegistration(eventClient, json, true, otherMemberPresenter);

    }
}
