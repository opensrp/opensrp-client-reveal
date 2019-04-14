package org.smartregister.reveal.presenter;


import org.smartregister.family.presenter.BaseFamilyRegisterPresenter;
import org.smartregister.reveal.contract.FamilyRegisterContract;

/**
 * Created by samuelgithengi on 4/14/19.
 */
public class FamilyRegisterPresenter extends BaseFamilyRegisterPresenter {

    private FamilyRegisterContract.View view;

    public FamilyRegisterPresenter(FamilyRegisterContract.View view, FamilyRegisterContract.Model model) {
        super(view, model);
        this.view = view;
    }


    @Override
    public void onRegistrationSaved(boolean isEdit) {
        view.hideProgressDialog();
        view.finish();

    }
}
