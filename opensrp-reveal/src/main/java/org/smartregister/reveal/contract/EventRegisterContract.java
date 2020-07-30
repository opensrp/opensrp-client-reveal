package org.smartregister.reveal.contract;

import org.smartregister.configurableviews.model.View;

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
    }
}
