package org.smartregister.reveal.contract;

import org.smartregister.reveal.model.OfflineMapModel;

import java.util.List;

public interface AvailableOfflineMapsContract {

    interface Presenter extends BaseContract.BasePresenter {

        void onDownloadMap();

        void onDownloadAreaSelected();
    }

    interface View {

        void setOfflineMapModelList(List<OfflineMapModel> offlineMapModelList);

        void displayToast(String message);

        void displayError(int title, String message);
    }
}
