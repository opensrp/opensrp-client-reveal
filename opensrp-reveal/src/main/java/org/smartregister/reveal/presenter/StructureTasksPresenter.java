package org.smartregister.reveal.presenter;

import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.interactor.StructureTasksInteractor;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.PreferencesUtil;

import java.util.List;

import static org.smartregister.reveal.contract.StructureTasksContract.Interactor;
import static org.smartregister.reveal.contract.StructureTasksContract.Presenter;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class StructureTasksPresenter implements Presenter {

    private StructureTasksContract.View view;

    private Interactor interactor;

    private PreferencesUtil prefsUtil;


    public StructureTasksPresenter(StructureTasksContract.View view) {
        this.view = view;
        interactor = new StructureTasksInteractor(this);
        prefsUtil = PreferencesUtil.getInstance();
    }

    @Override
    public void findTasks(String structureId) {
        interactor.findTasks(structureId, prefsUtil.getCurrentCampaignId());
    }

    @Override
    public void onTasksFound(List<StructureTaskDetails> taskDetailsList) {
        view.getAdapter().setTaskDetailsList(taskDetailsList);
    }
}
