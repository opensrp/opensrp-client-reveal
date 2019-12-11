package org.smartregister.reveal.contract;

import android.content.Context;
import android.support.annotation.NonNull;

import org.smartregister.clientandeventmodel.Client;
import org.smartregister.clientandeventmodel.Event;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public interface FamilyProfileContract extends org.smartregister.family.contract.FamilyProfileContract {

    interface View extends org.smartregister.family.contract.FamilyProfileContract.View {

        void setStructureId(String structureId);

        void refreshTasks(String structureId);

        void updateFamilyName(String firstName);

        Context getContext();
    }

    interface Interactor extends org.smartregister.family.contract.FamilyProfileContract.Interactor {

        void generateTasks(Context applicationContext, String baseEntityId, String structureId);

        void updateFamilyMemberName(@NonNull Client family, Event event, @NonNull String oldFamilyName);
    }

    interface Presenter extends org.smartregister.family.contract.FamilyProfileContract.Presenter {

        void onTasksGenerated();

        void onMembersUpdated();

        void onAddFamilyMember();

        void onArchiveFamilyClicked();
    }
}

