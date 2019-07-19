package org.smartregister.reveal.presenter;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.powermock.reflect.Whitebox;
import org.smartregister.Context;
import org.smartregister.commonregistry.CommonPersonObject;
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LAST_NAME;

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

    @Mock
    private FamilyOtherMemberProfileContract.Interactor otherMemberInteractor;

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
        Context.bindtypes = new ArrayList<>();
        otherMemberPresenter = new FamilyOtherMemberPresenter(view, model, null, familyBaseEntityId, baseEntityId, familyHead, primaryCaregiver, villageTown, familyName);
        Whitebox.setInternalState(otherMemberPresenter, "familyJsonFormUtils", familyJsonFormUtils);
        Whitebox.setInternalState(otherMemberPresenter, "interactor", interactor);
        Whitebox.setInternalState(otherMemberPresenter, "otherMemberInteractor", otherMemberInteractor);
    }

    @Test
    public void testStartFormForEdit() {
        client.setCaseId(familyHead);
        otherMemberPresenter.startFormForEdit(client);
        verify(familyJsonFormUtils, never()).getAutoPopulatedJsonEditMemberFormString(R.string.edit_member_form_title,
                RevealApplication.getInstance().getMetadata().familyMemberRegister.formName,
                client, RevealApplication.getInstance().getMetadata().familyMemberRegister.updateEventType, familyName);
        verify(view, never()).startFormActivity(any());
        verify(otherMemberInteractor).getFamilyHead(otherMemberPresenter, familyHead);
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

    @Test
    public void testUpdateFamilyMemberModelReturnsNull() {
        Whitebox.setInternalState(otherMemberPresenter, "profileInteractor", profileInteractor);
        Whitebox.setInternalState(otherMemberPresenter, "profileModel", profileModel);
        String json = "{}";
        when(profileModel.processUpdateMemberRegistration(json, familyBaseEntityId)).thenReturn(null);
        otherMemberPresenter.updateFamilyMember(json);
        verify(view).showProgressDialog(R.string.saving_dialog_title);
        verify(profileModel).processUpdateMemberRegistration(json, familyBaseEntityId);
        verifyNoMoreInteractions(profileInteractor);

    }

    @Test
    public void testUpdateFamilyMemberModelErrors() {
        Whitebox.setInternalState(otherMemberPresenter, "profileInteractor", profileInteractor);
        Whitebox.setInternalState(otherMemberPresenter, "profileModel", profileModel);
        String json = "{}";
        FamilyEventClient eventClient = new FamilyEventClient(null, null);
        doThrow(new RuntimeException()).when(profileModel).processUpdateMemberRegistration(json, familyBaseEntityId);
        otherMemberPresenter.updateFamilyMember(json);
        verify(view).showProgressDialog(R.string.saving_dialog_title);
        verify(profileModel).processUpdateMemberRegistration(json, familyBaseEntityId);
        verify(profileInteractor, never()).saveRegistration(eventClient, json, true, otherMemberPresenter);
        verify(view).hideProgressDialog();

    }

    @Test
    public void testOnFetchFamilyHead() {
        CommonPersonObject familyHead = new CommonPersonObject(UUID.randomUUID().toString(), null, null, null);
        Map<String, String> values = new HashMap<>();
        String surname = UUID.randomUUID().toString();
        values.put(LAST_NAME, surname);
        familyHead.setColumnmaps(values);
        JSONObject form = new JSONObject();
        when(familyJsonFormUtils.getAutoPopulatedJsonEditMemberFormString(R.string.edit_member_form_title,
                RevealApplication.getInstance().getMetadata().familyMemberRegister.formName,
                client, RevealApplication.getInstance().getMetadata().familyMemberRegister.updateEventType, surname)).thenReturn(form);
        Whitebox.setInternalState(otherMemberPresenter, "client", client);
        otherMemberPresenter.onFetchFamilyHead(familyHead);
        verify(view).startFormActivity(form);
        verify(familyJsonFormUtils).getAutoPopulatedJsonEditMemberFormString(R.string.edit_member_form_title,
                RevealApplication.getInstance().getMetadata().familyMemberRegister.formName,
                client, RevealApplication.getInstance().getMetadata().familyMemberRegister.updateEventType, surname);
    }


    @Test
    public void testOnUniqueIdFetched() {
        otherMemberPresenter.onUniqueIdFetched(null, null);
        verify(view).getContext();
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);
    }


    @Test
    public void testOnNoUniqueId() {
        otherMemberPresenter.onNoUniqueId();
        verify(view).getContext();
        verifyNoMoreInteractions(view);
        verifyNoMoreInteractions(interactor);
    }

    @Test
    public void testOnEditMemberDetails() {
        otherMemberPresenter.onEditMemberDetails();
        verify(view, never()).startFormActivity(any());
        verify(otherMemberInteractor).getFamilyHead(otherMemberPresenter, familyHead);
    }
}