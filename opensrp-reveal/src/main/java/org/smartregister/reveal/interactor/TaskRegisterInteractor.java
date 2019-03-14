package org.smartregister.reveal.interactor;

import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.configurableviews.helper.ConfigurableViewsHelper;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.TaskRegisterContract;
import org.smartregister.reveal.util.AppExecutors;

import java.util.List;

/**
 * Created by samuelgithengi on 3/14/19.
 */
public class TaskRegisterInteractor implements TaskRegisterContract.Interactor {

    private ConfigurableViewsHelper viewsHelper;

    private AppExecutors appExecutors;

    public TaskRegisterInteractor() {
        viewsHelper = ConfigurableViewsLibrary.getInstance().getConfigurableViewsHelper();
        appExecutors = RevealApplication.getInstance().getAppExecutors();
    }

    @Override
    public void registerViewConfigurations(List<String> viewIdentifiers) {
        appExecutors.diskIO().execute(() -> {
            viewsHelper.registerViewConfigurations(viewIdentifiers);
        });
    }

    @Override
    public void unregisterViewConfiguration(List<String> viewIdentifiers) {
        viewsHelper.unregisterViewConfiguration(viewIdentifiers);
    }

    @Override
    public void cleanupResources() {
        viewsHelper = null;
        appExecutors = null;
    }


}
