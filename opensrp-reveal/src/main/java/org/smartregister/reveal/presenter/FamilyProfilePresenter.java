package org.smartregister.reveal.presenter;

import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.contract.FamilyProfileContract;
import org.smartregister.family.presenter.BaseFamilyProfilePresenter;

/**
 * Created by samuelgithengi on 4/10/19.
 */
public class FamilyProfilePresenter extends BaseFamilyProfilePresenter {
    public FamilyProfilePresenter(FamilyProfileContract.View loginView, FamilyProfileContract.Model model, String familyBaseEntityId, String familyHead, String primaryCaregiver, String familyName) {
        super(loginView, model, familyBaseEntityId, familyHead, primaryCaregiver, familyName);
    }

    @Override
    public void refreshProfileTopSection(CommonPersonObjectClient client) {
        super.refreshProfileTopSection(client);
        getView().setProfileDetailOne("MTI_13");
        getView().setProfileDetailTwo("Chadiza");
    }
}
