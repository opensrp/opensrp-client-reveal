package org.smartregister.reveal.contract;

import org.smartregister.domain.Event;
import org.smartregister.view.contract.BaseRegisterFragmentContract;

import java.util.Set;

/**
 * Created by samuelgithengi on 7/30/20.
 */
public interface EventRegisterContract {
    interface View {

        void initializeQueryParams(String tableName, String countSelect, String mainSelect);

        void initializeAdapter(Set<org.smartregister.configurableviews.model.View> visibleColumns);

        void countExecute();

        void filterandSortInInitializeQueries();

        void startMapActivity();
    }

    interface Presenter extends BaseRegisterFragmentContract.Presenter {
        void onEventFound(Event event);

        void onOpenMapClicked();
    }

    interface Interactor {
        void findEvent(String formSubmissionId);
    }
}
