package org.smartregister.reveal.contract;

import java.util.List;

public interface ChildRegisterContract {


    interface Interactor {

        void registerViewConfigurations(List<String> viewIdentifiers);

        void unregisterViewConfiguration(List<String> viewIdentifiers);

    }
}
