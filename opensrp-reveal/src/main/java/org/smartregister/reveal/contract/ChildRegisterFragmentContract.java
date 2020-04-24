package org.smartregister.reveal.contract;

import android.support.annotation.Nullable;
import android.support.annotation.WorkerThread;

import org.smartregister.reveal.model.Child;
import org.smartregister.view.ListContract;

import java.util.HashMap;
import java.util.List;

public interface ChildRegisterFragmentContract {

    interface View extends ListContract.View<Child> {
        void openFilterFragment();

        void startChildRegistrationForm();
    }

    interface Presenter extends ListContract.Presenter<Child> {

        void search(String schoolID, @Nullable HashMap<String, List<String>> sortAndFilter, @Nullable String searchText);
    }

    interface Model extends ListContract.Model<Child> {

        @WorkerThread
        List<Child> searchAndFilter(String schoolID, @Nullable HashMap<String, List<String>> sortAndFilter, @Nullable String searchText);

    }

}
