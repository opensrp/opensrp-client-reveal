package org.smartregister.reveal.presenter;

import org.smartregister.reveal.contract.ChildRegisterFragmentContract;
import org.smartregister.reveal.model.Child;
import org.smartregister.reveal.model.ChildModel;
import org.smartregister.util.AppExecutors;
import org.smartregister.view.presenter.ListPresenter;

import java.util.Map;

public class ChildRegisterFragmentPresenter extends ListPresenter<Child> implements ChildRegisterFragmentContract.Presenter {

    private String sortOrder = "";

    @Override
    public void filterAndSort(Map<String, String> filter, String sortArgs) {
        ChildModel model = getModel();
        sortOrder = sortArgs;
        this.fetchList(() -> model.filterChildren(filter, sortArgs), AppExecutors.Request.DISK_THREAD);
    }

    @Override
    public void search(String searchText) {
        ChildModel model = getModel();
        this.fetchList(() -> model.searchChildren(searchText, sortOrder), AppExecutors.Request.DISK_THREAD);
    }

    @Override
    public void setSortDirection(String sortArgs) {
        sortOrder = sortArgs;
    }
}
