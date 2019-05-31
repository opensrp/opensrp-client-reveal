package org.smartregister.reveal.presenter;


import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.contract.FamilyOtherMemberContract.Model;
import org.smartregister.family.presenter.BaseFamilyOtherMemberProfileActivityPresenter;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.util.FamilyJsonFormUtils;

import timber.log.Timber;

/**
 * Created by samuelgithengi on 5/31/19.
 */
public class FamilyOtherMemberPresenter extends BaseFamilyOtherMemberProfileActivityPresenter implements FamilyOtherMemberProfileContract.Presenter {

    private CommonPersonObjectClient client;

    private FamilyJsonFormUtils familyJsonFormUtils;

    private String familyName;

    public FamilyOtherMemberPresenter(FamilyOtherMemberProfileContract.View view, Model model,
                                      String viewConfigurationIdentifier, String baseEntityId,
                                      String familyHead, String primaryCaregiver, String villageTown, String familyName) {
        super(view, model, viewConfigurationIdentifier, baseEntityId, familyHead, primaryCaregiver, villageTown);
        this.familyName = familyName;
        try {
            familyJsonFormUtils = new FamilyJsonFormUtils(view.getContext());
        } catch (Exception e) {
            Timber.e("error starting FamilyJsonFormUtils");
        }
    }


    @Override
    public void refreshProfileTopSection(CommonPersonObjectClient client) {
        super.refreshProfileTopSection(client);
        this.client = client;
    }

    @Override
    public void onEditMemberDetails() {

        JSONObject form = familyJsonFormUtils.getAutoPopulatedJsonEditMemberFormString(
                R.string.edit_member_form_title,
                RevealApplication.getInstance().getMetadata().familyMemberRegister.formName,
                client, RevealApplication.getInstance().getMetadata().familyMemberRegister.updateEventType, familyName);
        getView().startFormActivity(form);
    }

    @Override
    public void updateFamilyMember(String jsonString) {

    }

    @Override
    protected FamilyOtherMemberProfileContract.View getView() {
        return (FamilyOtherMemberProfileContract.View) super.getView();
    }
}
