package org.smartregister.reveal.contract;

import android.content.Context;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public interface FamilyProfileContract extends org.smartregister.family.contract.FamilyProfileContract {

    interface View extends org.smartregister.family.contract.FamilyProfileContract.View {

        void setStructureId(String structureId);

        void refreshTasks(String structureId);
    }

    interface Interactor extends org.smartregister.family.contract.FamilyProfileContract.Interactor {

        void generateTasks(Context applicationContext, String baseEntityId);
    }

    interface Presenter extends org.smartregister.family.contract.FamilyProfileContract.Presenter {

        void onTasksGenerated();
    }
}

