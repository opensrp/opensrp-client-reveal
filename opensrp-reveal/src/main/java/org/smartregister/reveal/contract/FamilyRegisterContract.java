package org.smartregister.reveal.contract;

import android.content.Context;

import org.smartregister.family.domain.FamilyEventClient;

import java.util.List;

/**
 * Created by samuelgithengi on 4/14/19.
 */
public interface FamilyRegisterContract extends org.smartregister.family.contract.FamilyRegisterContract {

    interface View extends org.smartregister.family.contract.FamilyRegisterContract.View {

        void startProfileActivity(String baseEntityId, String familyHead, String primaryCareGiver,
                                  String cityVillage, String firstName);
    }


    interface Interactor extends org.smartregister.family.contract.FamilyRegisterContract.Interactor {

        void generateTasks(List<FamilyEventClient> eventClientList, String structureId, Context context);
    }


    interface Presenter extends org.smartregister.family.contract.FamilyRegisterContract.Presenter {

        void onTasksGenerated();
    }
}
