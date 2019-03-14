package org.smartregister.reveal.contract;

import java.util.List;

/**
 * Created by samuelgithengi on 3/14/19.
 */
public interface TaskRegisterContract {

    interface Interactor {

        void registerViewConfigurations(List<String> viewIdentifiers);

        void unregisterViewConfiguration(List<String> viewIdentifiers);

        void cleanupResources();
    }
}
