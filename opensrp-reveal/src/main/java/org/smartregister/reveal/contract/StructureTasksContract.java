package org.smartregister.reveal.contract;

import org.smartregister.reveal.adapter.StructureTaskAdapter;
import org.smartregister.reveal.model.StructureTaskDetails;

import java.util.List;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public interface StructureTasksContract {

    interface Presenter {

        void findTasks(String structureId);

        void onTasksFound(List<StructureTaskDetails> taskDetailsList);
    }

    interface Interactor {

        void findTasks(String structureId);

    }

    interface View {

        StructureTaskAdapter getAdapter();

        void setStructure(String structureId);
    }
}
