package org.smartregister.reveal.presenter;

import org.smartregister.family.contract.FamilyRegisterContract;
import org.smartregister.family.presenter.BaseFamilyRegisterPresenter;

/**
 * Created by samuelgithengi on 2/8/19.
 */
public class FamilyRegisterPresenter extends BaseFamilyRegisterPresenter {

    public FamilyRegisterPresenter(FamilyRegisterContract.View view, FamilyRegisterContract.Model model) {
        super(view, model);
    }
}
