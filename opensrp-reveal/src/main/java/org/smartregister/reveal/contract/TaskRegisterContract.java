package org.smartregister.reveal.contract;

import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Task;
import org.smartregister.view.contract.BaseRegisterContract;

import java.util.List;

/**
 * Created by samuelgithengi on 3/14/19.
 */
public interface TaskRegisterContract {

    interface Presenter extends BaseContract.BasePresenter {
        void saveJsonForm(String json);

        void onFamilyFound(CommonPersonObjectClient finalFamily);
    }

    interface Interactor {

        void registerViewConfigurations(List<String> viewIdentifiers);

        void unregisterViewConfiguration(List<String> viewIdentifiers);

        void cleanupResources();
    }

    interface View extends BaseRegisterContract.View {
        void displayNotification(int title, int message);

        void openStructureProfile(CommonPersonObjectClient family, Task task, String structureId);
    }


}
