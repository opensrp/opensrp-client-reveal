package org.smartregister.reveal.presenter;

import android.support.annotation.Nullable;

import org.smartregister.reveal.contract.ChildRegisterFragmentContract;
import org.smartregister.reveal.model.Child;
import org.smartregister.reveal.model.ChildModel;
import org.smartregister.util.AppExecutors;
import org.smartregister.view.presenter.ListPresenter;

import java.util.HashMap;
import java.util.List;

public class ChildRegisterFragmentPresenter extends ListPresenter<Child> implements ChildRegisterFragmentContract.Presenter {

    @Override
    public void search(String schoolID, @Nullable HashMap<String, List<String>> sortAndFilter, @Nullable String searchText) {
        ChildModel model = getModel();
        this.fetchList(() -> model.searchAndFilter(schoolID, sortAndFilter, searchText), AppExecutors.Request.DISK_THREAD);
    }

}
