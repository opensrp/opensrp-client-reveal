package org.smartregister.reveal.presenter;

import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.model.StructureTaskDetails;

import java.util.List;

import static org.smartregister.reveal.contract.StructureTasksContract.Interactor;
import static org.smartregister.reveal.contract.StructureTasksContract.Presenter;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class StructureTasksPresenter implements Presenter {

    private StructureTasksContract.View view;

    private Interactor interactor;


    public StructureTasksPresenter(StructureTasksContract.View view) {
        this.view = view;
    }

    @Override
    public void findTasks(String structureId) {
        interactor.findTasks(structureId);
    }

    @Override
    public void onTasksFound(List<StructureTaskDetails> taskDetailsList) {
        view.getAdapter().setTaskDetailsList(taskDetailsList);
    }
}
