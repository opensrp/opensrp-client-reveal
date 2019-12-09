package org.smartregister.reveal.presenter;


import android.app.Activity;
import android.content.DialogInterface;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Triple;
import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.contract.FamilyOtherMemberContract.Model;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.interactor.FamilyProfileInteractor;
import org.smartregister.family.presenter.BaseFamilyOtherMemberProfileActivityPresenter;
import org.smartregister.family.util.DBConstants;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.interactor.RevealFamilyOtherMemberInteractor;
import org.smartregister.reveal.model.FamilyProfileModel;
import org.smartregister.reveal.util.AlertDialogUtils;
import org.smartregister.reveal.util.FamilyJsonFormUtils;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.LAST_NAME;

/**
 * Created by samuelgithengi on 5/31/19.
 */
public class FamilyOtherMemberPresenter extends BaseFamilyOtherMemberProfileActivityPresenter
        implements FamilyOtherMemberProfileContract.Presenter, FamilyProfileContract.InteractorCallBack {


    private CommonPersonObjectClient client;

    private FamilyJsonFormUtils familyJsonFormUtils;

    private String familyBaseEntityId;
    private String familyName;

    private FamilyOtherMemberProfileContract.Interactor otherMemberInteractor;
    private org.smartregister.family.contract.FamilyProfileContract.Interactor profileInteractor;
    private FamilyProfileContract.Model profileModel;

    public FamilyOtherMemberPresenter(FamilyOtherMemberProfileContract.View view, Model model,
                                      String viewConfigurationIdentifier, String familyBaseEntityId, String baseEntityId,
                                      String familyHead, String primaryCaregiver, String villageTown, String familyName) {
        super(view, model, viewConfigurationIdentifier, baseEntityId, familyHead, primaryCaregiver, villageTown);
        this.familyBaseEntityId = familyBaseEntityId;
        this.familyName = familyName;
        this.otherMemberInteractor = new RevealFamilyOtherMemberInteractor();
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
        if (StringUtils.isBlank(familyHead)) {
            startFamilyMemberForm(familyName, false);
        } else if (client.getCaseId().equals(familyHead)) {
            startFamilyMemberForm(familyName, true);
        } else {
            otherMemberInteractor.getFamilyHead(this, familyHead);
        }
    }

    @Override
    public void onFetchFamilyHead(CommonPersonObject familyHeadPersonObject) {
        startFamilyMemberForm(familyHeadPersonObject.getColumnmaps().get(LAST_NAME), false);
    }

    @Override
    public void onArchiveMemberCompleted(boolean isSuccessful) {
        getView().hideProgressDialog();
        if (!isSuccessful) {
            AlertDialogUtils.displayNotification(getView().getContext(), R.string.archive_member,
                    R.string.archive_member_failed, client.getColumnmaps().get(DBConstants.KEY.FIRST_NAME),
                    client.getColumnmaps().get(DBConstants.KEY.LAST_NAME));
        } else {
            finishActivity();
        }
    }

    private void finishActivity() {
        ((Activity) getView().getContext()).finish();
    }

    private void startFamilyMemberForm(String familyName, boolean isFamilyHead) {
        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditMemberFormString(
                R.string.edit_member_form_title,
                RevealApplication.getInstance().getMetadata().familyMemberRegister.formName,
                client, RevealApplication.getInstance().getMetadata().familyMemberRegister.updateEventType,
                familyName, isFamilyHead);
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
    public void onRegistrationSaved(boolean isEditMode, boolean isSaved, FamilyEventClient familyEventClient) {
        if (isEditMode) {
            getView().hideProgressDialog();

            refreshProfileView();

            getView().refreshList();

        }
        RevealApplication.getInstance().setRefreshMapOnEventSaved(true);

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
    public void onArchiveFamilyMember() {
        AlertDialogUtils.displayNotificationWithCallback(getView().getContext(), R.string.archive_member, R.string.confirm_archive_member, R.string.confirm, R.string.cancel, (dialog, buttonClicked) -> {
            if (buttonClicked == DialogInterface.BUTTON_POSITIVE) {
                archiveFamilyMember();
            }
            dialog.dismiss();
        });
    }

    private void archiveFamilyMember() {
        getView().showProgressDialog(org.smartregister.family.R.string.saving_dialog_title);
        otherMemberInteractor.archiveFamilyMember(this, client);
    }

    @Override
    protected FamilyOtherMemberProfileContract.View getView() {
        return (FamilyOtherMemberProfileContract.View) super.getView();
    }

    public void setStructureId(String structureId) {
        ((FamilyProfileModel) profileModel).setStructureId(structureId);
    }
}
