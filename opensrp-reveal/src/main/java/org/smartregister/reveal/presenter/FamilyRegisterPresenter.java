package org.smartregister.reveal.presenter;


import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.presenter.BaseFamilyRegisterPresenter;
import org.smartregister.reveal.contract.FamilyRegisterContract;
import org.smartregister.reveal.model.FamilyRegisterModel;
import org.smartregister.reveal.util.TaskUtils;

import java.util.HashSet;
import java.util.Set;

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
        generateTasks();
    }

    private void generateTasks() {
        Set<String> generatedIds = new HashSet<>();
        for (FamilyEventClient eventClient : getModel().getEventClientList()) {
            if (eventClient.getClient().getLastName().equals("Family"))
                continue;
            String entityId = eventClient.getClient().getBaseEntityId();
            if (!generatedIds.contains(entityId)) {
                generatedIds.add(entityId);
                TaskUtils.generateBloodScreeningTask(view.getContext(), entityId);
            }
        }
        TaskUtils.generateBedNetDistributionTask(view.getContext(), getModel().getStructureId());
    }

    private FamilyRegisterModel getModel() {
        return (FamilyRegisterModel) model;
    }
}
