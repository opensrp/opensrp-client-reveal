package org.smartregister.reveal.presenter;


import org.smartregister.clientandeventmodel.Client;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.presenter.BaseFamilyRegisterPresenter;
import org.smartregister.reveal.contract.FamilyRegisterContract;
import org.smartregister.reveal.interactor.RevealFamilyRegisterInteractor;
import org.smartregister.reveal.model.FamilyRegisterModel;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.TaskUtils;

/**
 * Created by samuelgithengi on 4/14/19.
 */
public class FamilyRegisterPresenter extends BaseFamilyRegisterPresenter implements FamilyRegisterContract.Presenter {

    private FamilyRegisterContract.View view;

    private TaskUtils taskUtils;

    public FamilyRegisterPresenter(FamilyRegisterContract.View view, FamilyRegisterContract.Model model) {
        super(view, model);
        this.view = view;
        taskUtils = TaskUtils.getInstance();
        setInteractor(new RevealFamilyRegisterInteractor(this));
    }


    @Override
    public void onRegistrationSaved(boolean isEdit) {
        getInteractor().generateTasks(getModel().getEventClientList(), getModel().getStructureId(), view.getContext());

    }

    @Override
    public void onTasksGenerated() {
        view.hideProgressDialog();
        openProfileActivity();
    }

    private void openProfileActivity() {
        for (FamilyEventClient eventClient : getModel().getEventClientList()) {
            if (eventClient.getClient().getLastName().equals("Family")) {
                Client family = eventClient.getClient();
                view.startProfileActivity(family.getBaseEntityId(),
                        family.findRelatives(FamilyConstants.RELATIONSHIP.FAMILY_HEAD).get(0),
                        family.findRelatives(FamilyConstants.RELATIONSHIP.PRIMARY_CAREGIVER).get(0),
                        family.getAddress("").getCityVillage(),
                        family.getFirstName());
                break;
            }
        }
    }

    private FamilyRegisterModel getModel() {
        return (FamilyRegisterModel) model;
    }

    private FamilyRegisterContract.Interactor getInteractor() {
        return (FamilyRegisterContract.Interactor) interactor;
    }
}
