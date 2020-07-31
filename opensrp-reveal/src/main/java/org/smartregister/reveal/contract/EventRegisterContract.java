package org.smartregister.reveal.contract;

import org.json.JSONObject;
import org.smartregister.domain.Event;
import org.smartregister.reveal.model.EventRegisterDetails;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.view.contract.BaseRegisterFragmentContract;

import java.util.Set;

/**
 * Created by samuelgithengi on 7/30/20.
 */
public interface EventRegisterContract {
    interface View extends BaseRegisterFragmentContract.View {

        void initializeQueryParams(String tableName, String countSelect, String mainSelect);

        void initializeAdapter(Set<org.smartregister.configurableviews.model.View> visibleColumns);

        void countExecute();

        void filterandSortInInitializeQueries();

        RevealJsonFormUtils getJsonFormUtils();

        void startForm(JSONObject formName);

        void displayError(int title, int message);

    }

    interface Presenter extends  BaseRegisterFragmentContract.Presenter {
        void onEventFound(Event event);

        void onEventSelected(EventRegisterDetails details);
    }

    interface Interactor {
        void findEvent(String formSubmissionId);
    }
}
