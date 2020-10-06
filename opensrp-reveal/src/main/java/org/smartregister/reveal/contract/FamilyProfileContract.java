package org.smartregister.reveal.contract;

import android.app.Activity;
import android.content.Context;
import androidx.annotation.NonNull;

import org.json.JSONObject;
import org.smartregister.clientandeventmodel.Client;
import org.smartregister.clientandeventmodel.Event;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Task;

import java.util.Date;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public interface FamilyProfileContract extends org.smartregister.family.contract.FamilyProfileContract {

    interface View extends org.smartregister.family.contract.FamilyProfileContract.View {

        void startFormActivity(JSONObject jsonObject, boolean readOnly);

        void setStructureId(String structureId);

        void refreshTasks(String structureId);

        void updateFamilyName(String firstName);

        Activity getContext();

        void returnToMapView(String structureId, Task task);
    }

    interface Interactor extends org.smartregister.family.contract.FamilyProfileContract.Interactor {

        void generateTasks(Context applicationContext, String baseEntityId, String structureId, Date birthDate);

        void updateFamilyMemberName(@NonNull Client family, Event event, @NonNull String oldFamilyName);

        void archiveFamily(String familyBaseEntityId, String structureId);

        void getRegistrationEvent(CommonPersonObjectClient client, String familyHead);
    }

    interface Presenter extends org.smartregister.family.contract.FamilyProfileContract.Presenter {

        void onTasksGenerated();

        void onMembersUpdated();

        void onAddFamilyMember();

        void onArchiveFamilyClicked();

        void onArchiveFamilyCompleted(boolean isSuccessfulSaved, Task task);

        void onEventFound(org.smartregister.domain.Event structureEvent, CommonPersonObjectClient client);
    }
}

