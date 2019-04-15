package org.smartregister.reveal.presenter;


import org.smartregister.clientandeventmodel.Client;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.presenter.BaseFamilyRegisterPresenter;
import org.smartregister.reveal.contract.FamilyRegisterContract;
import org.smartregister.reveal.interactor.RevealFamilyRegisterInteractor;
import org.smartregister.reveal.model.FamilyRegisterModel;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.TaskUtils;

import java.util.HashSet;
import java.util.Set;

/**
 * Created by samuelgithengi on 4/14/19.
 */
public class FamilyRegisterPresenter extends BaseFamilyRegisterPresenter {

    private FamilyRegisterContract.View view;

    private TaskUtils taskUtils;

    public FamilyRegisterPresenter(FamilyRegisterContract.View view, FamilyRegisterContract.Model model) {
        super(view, model);
        this.view = view;
        taskUtils = TaskUtils.getInstance();
        setInteractor(new RevealFamilyRegisterInteractor());
    }


    @Override
    public void onRegistrationSaved(boolean isEdit) {
        view.hideProgressDialog();
        openProfileActivity();
        generateTasks();
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

    private void generateTasks() {
        Set<String> generatedIds = new HashSet<>();
        for (FamilyEventClient eventClient : getModel().getEventClientList()) {
            if (eventClient.getClient().getLastName().equals("Family"))
                continue;
            String entityId = eventClient.getClient().getBaseEntityId();
            if (!generatedIds.contains(entityId)) {
                generatedIds.add(entityId);
                taskUtils.generateBloodScreeningTask(view.getContext(), entityId);
            }
        }
        taskUtils.generateBedNetDistributionTask(view.getContext(), getModel().getStructureId());
    }


    private FamilyRegisterModel getModel() {
        return (FamilyRegisterModel) model;
    }
}
