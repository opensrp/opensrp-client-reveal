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

        void onTaskSelected(StructureTaskDetails details);
    }

    interface Interactor {

        void findTasks(String structureId, String currentCampaignId);

        void getStructure(StructureTaskDetails details);
    }

    interface View {

        StructureTaskAdapter getAdapter();

        void setStructure(String structureId);

        void displayToast(String message);

        void showProgressDialog(int title, int message);

        void hideProgressDialog();
    }
}
