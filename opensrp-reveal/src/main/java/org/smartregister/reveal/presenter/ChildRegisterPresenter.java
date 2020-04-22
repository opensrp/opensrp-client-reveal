package org.smartregister.reveal.presenter;

import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.configurableviews.helper.ConfigurableViewsHelper;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.util.Utils;
import org.smartregister.view.activity.BaseRegisterActivity;
import org.smartregister.view.contract.BaseRegisterContract;

import java.lang.ref.WeakReference;
import java.util.List;

public class ChildRegisterPresenter implements BaseRegisterContract.Presenter {

    private AppExecutors appExecutors;
    private ConfigurableViewsHelper viewsHelper;
    protected WeakReference<BaseRegisterActivity> viewReference;

    public ChildRegisterPresenter(BaseRegisterActivity activity) {
        viewsHelper = ConfigurableViewsLibrary.getInstance().getConfigurableViewsHelper();
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        viewReference = new WeakReference<>(activity);
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
    public void onDestroy(boolean b) {
        // do nothing
    }

    @Override
    public void updateInitials() {
        String initials = Utils.getUserInitials();
        if (getView() != null) {
            getView().updateInitialsText(initials);
        }
    }

    private BaseRegisterActivity getView() {
        if (viewReference != null)
            return viewReference.get();
        else
            return null;
    }
}
