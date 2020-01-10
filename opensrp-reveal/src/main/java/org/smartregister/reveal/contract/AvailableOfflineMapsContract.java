package org.smartregister.reveal.contract;

import org.smartregister.reveal.model.OfflineMapModel;

import java.util.List;

public interface AvailableOfflineMapsContract extends OfflineMapsFragmentContract {

    interface Presenter {

        void fetchAvailableOAsForMapDownLoad(List<String> locationIds);

        void onFetchAvailableOAsForMapDownLoad(List<OfflineMapModel> offlineMapModels);

        void onDownloadStarted(String operationalAreaId);

        void onDownloadComplete(String operationalAreaId);

    }

    interface View {

        void setOfflineMapModelList(List<OfflineMapModel> offlineMapModelList, boolean initialLoad);

        void disableCheckBox(String operationalAreaId);

        void moveDownloadedOAToDownloadedList(String operationalAreaId);
    }

    interface Interactor {

        void fetchAvailableOAsForMapDownLoad(List<String> locationIds);
    }
}
