package org.smartregister.reveal.presenter;

import org.smartregister.domain.Task;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.interactor.StructureTasksInteractor;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.PreferencesUtil;

import java.lang.ref.WeakReference;
import java.util.List;

import static org.smartregister.reveal.contract.StructureTasksContract.Interactor;
import static org.smartregister.reveal.contract.StructureTasksContract.Presenter;
import static org.smartregister.reveal.util.Constants.Intervention.BCC;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class StructureTasksPresenter implements Presenter {

    private WeakReference<StructureTasksContract.View> view;

    private Interactor interactor;

    private PreferencesUtil prefsUtil;


    public StructureTasksPresenter(StructureTasksContract.View view) {
        this.view = new WeakReference<>(view);
        interactor = new StructureTasksInteractor(this);
        prefsUtil = PreferencesUtil.getInstance();
    }

    @Override
    public void findTasks(String structureId) {
        interactor.findTasks(structureId, prefsUtil.getCurrentCampaignId());
    }

    @Override
    public void onTasksFound(List<StructureTaskDetails> taskDetailsList) {
        getView().getAdapter().setTaskDetailsList(taskDetailsList);
    }

    @Override
    public void onTaskSelected(StructureTaskDetails details) {
        if (details != null) {
            //TODO remove this condition once BCC and Larval dipping forms are implemented
            if (BCC.equals(details.getTaskCode()) || LARVAL_DIPPING.equals(details.getTaskCode())) {
                getView().displayToast(String.format("To open %s form for %s",
                        details.getTaskCode(), details.getTaskId()));

            } else if (Task.TaskStatus.COMPLETED.name().equals(details.getTaskStatus())) {
                getView().displayToast("Task Completed");
            } else {
                getView().showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
                interactor.getStructure(details);
            }
        }
    }

    public StructureTasksContract.View getView() {
        return view.get();
    }
}
