package org.smartregister.reveal.contract;

import android.support.annotation.WorkerThread;

import org.smartregister.reveal.model.Child;
import org.smartregister.view.ListContract;

import java.util.List;
import java.util.Map;

public interface ChildRegisterFragmentContract {

    interface View extends ListContract.View<Child> {

    }

    interface Presenter extends ListContract.Presenter<Child> {

        void filterAndSort(Map<String, String> filter, String sortArgs);

        void search(String searchText);

        void setSortDirection(String sortArgs);
    }

    interface Model extends ListContract.Model<Child> {

        @WorkerThread
        List<Child> filterChildren(Map<String, String> filterArgs, String sortArgs);

        @WorkerThread
        List<Child> searchChildren(String searchText, String sortArgs);

    }

}
