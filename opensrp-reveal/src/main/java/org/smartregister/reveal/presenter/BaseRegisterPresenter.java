package org.smartregister.reveal.presenter;

import org.smartregister.reveal.interactor.TaskRegisterInteractor;
import org.smartregister.view.contract.BaseRegisterContract;

import java.util.List;

/**
 * Created by samuelgithengi on 7/30/20.
 */
public class BaseRegisterPresenter implements BaseRegisterContract.Presenter {

    protected TaskRegisterInteractor taskRegisterInteractor;

    public BaseRegisterPresenter(BaseRegisterContract.View view) {
        taskRegisterInteractor = new TaskRegisterInteractor(null);
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
    public void updateInitials() {//do nothing
    }
}
