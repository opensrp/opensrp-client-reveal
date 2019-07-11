package org.smartregister.reveal.presenter;


import org.apache.commons.lang3.tuple.Triple;
import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.contract.FamilyOtherMemberContract.Model;
import org.smartregister.family.contract.FamilyProfileContract;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.interactor.FamilyProfileInteractor;
import org.smartregister.family.presenter.BaseFamilyOtherMemberProfileActivityPresenter;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.model.FamilyProfileModel;
import org.smartregister.reveal.util.FamilyJsonFormUtils;

import timber.log.Timber;

/**
 * Created by samuelgithengi on 5/31/19.
 */
public class FamilyOtherMemberPresenter extends BaseFamilyOtherMemberProfileActivityPresenter
        implements FamilyOtherMemberProfileContract.Presenter, FamilyProfileContract.InteractorCallBack {


    private CommonPersonObjectClient client;

    private FamilyJsonFormUtils familyJsonFormUtils;

    private String familyBaseEntityId;
    private String familyName;

    private FamilyProfileContract.Interactor profileInteractor;
    private FamilyProfileContract.Model profileModel;

    public FamilyOtherMemberPresenter(FamilyOtherMemberProfileContract.View view, Model model,
                                      String viewConfigurationIdentifier, String familyBaseEntityId, String baseEntityId,
                                      String familyHead, String primaryCaregiver, String villageTown, String familyName) {
        super(view, model, viewConfigurationIdentifier, baseEntityId, familyHead, primaryCaregiver, villageTown);
        this.familyBaseEntityId = familyBaseEntityId;
        this.familyName = familyName;
        this.profileInteractor = new FamilyProfileInteractor();
        this.profileModel = new FamilyProfileModel(familyName);
        try {
            familyJsonFormUtils = new FamilyJsonFormUtils(view.getContext());
        } catch (Exception e) {
            Timber.e("error starting FamilyJsonFormUtils");
        }
    }


    @Override
    public void startFormForEdit(CommonPersonObjectClient client) {
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditMemberFormString(
                R.string.edit_member_form_title,
                RevealApplication.getInstance().getMetadata().familyMemberRegister.formName,
                client, RevealApplication.getInstance().getMetadata().familyMemberRegister.updateEventType, familyName);
        getView().startFormActivity(form);
    }

    @Override
    public void refreshProfileTopSection(CommonPersonObjectClient client) {
        super.refreshProfileTopSection(client);
        this.client = client;
    }

    @Override
    public void onUniqueIdFetched(Triple<String, String, String> triple, String entityId) {//not used
    }

    @Override
    public void onNoUniqueId() {//not used
    }

    @Override
    public void onRegistrationSaved(boolean isEditMode) {
        if (isEditMode) {
            getView().hideProgressDialog();

            refreshProfileView();

            getView().refreshList();
        }

    }

    @Override
    public void onEditMemberDetails() {
        startFormForEdit(client);
    }

    @Override
    public void updateFamilyMember(String jsonString) {
        try {
            getView().showProgressDialog(org.smartregister.family.R.string.saving_dialog_title);

            FamilyEventClient familyEventClient = profileModel.processUpdateMemberRegistration(jsonString, familyBaseEntityId);
            if (familyEventClient == null) {
                return;
            }

            profileInteractor.saveRegistration(familyEventClient, jsonString, true, this);
        } catch (Exception e) {
            getView().hideProgressDialog();
            Timber.e(e);
        }
    }

    @Override
    protected FamilyOtherMemberProfileContract.View getView() {
        return (FamilyOtherMemberProfileContract.View) super.getView();
    }
}
