package org.smartregister.reveal.presenter;

import org.smartregister.reveal.interactor.TaskRegisterInteractor;

import java.util.List;

import static org.smartregister.view.contract.BaseRegisterContract.Presenter;

/**
 * Created by samuelgithengi on 3/11/19.
 */
public class TaskRegisterPresenter implements Presenter {


    private TaskRegisterInteractor taskRegisterInteractor;

    public TaskRegisterPresenter() {
        taskRegisterInteractor = new TaskRegisterInteractor();
    }

    @Override
    public void registerViewConfigurations(List<String> viewIdentifiers) {
        taskRegisterInteractor.registerViewConfigurations(viewIdentifiers);
    }

    @Override
    public void unregisterViewConfiguration(List<String> viewIdentifiers) {
        taskRegisterInteractor.unregisterViewConfiguration(viewIdentifiers);
    }

    @Override
    public void onDestroy(boolean isChangingConfiguration) {
        taskRegisterInteractor.cleanupResources();
    }

    @Override
    public void updateInitials() {
    }
}
