package org.smartregister.reveal.presenter;

import org.smartregister.family.contract.FamilyRegisterFragmentContract;
import org.smartregister.family.presenter.BaseFamilyRegisterFragmentPresenter;
import org.smartregister.family.util.DBConstants;

public class FamilyRegisterFragmentPresenter extends BaseFamilyRegisterFragmentPresenter {

    public FamilyRegisterFragmentPresenter(FamilyRegisterFragmentContract.View view, FamilyRegisterFragmentContract.Model model, String viewConfigurationIdentifier) {
        super(view, model, viewConfigurationIdentifier);
    }

    @Override
    public String getMainCondition() {
        return String.format(" %s is null ", DBConstants.KEY.DATE_REMOVED);
    }

    @Override
    public String getDefaultSortQuery() {
        return DBConstants.KEY.LAST_INTERACTED_WITH + " DESC ";
    }

}
