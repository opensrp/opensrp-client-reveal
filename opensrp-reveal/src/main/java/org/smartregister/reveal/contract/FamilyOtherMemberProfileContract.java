package org.smartregister.reveal.contract;

import android.content.Context;

import org.json.JSONObject;
import org.smartregister.family.contract.FamilyOtherMemberContract;

/**
 * Created by samuelgithengi on 5/31/19.
 */
public interface FamilyOtherMemberProfileContract {

    interface Presenter extends FamilyOtherMemberContract.Presenter {
        void onEditMemberDetails();

        void updateFamilyMember(String jsonString);
    }

    interface View extends FamilyOtherMemberContract.View {

        void startFormActivity(JSONObject jsonForm);

        Context getContext();

        void refreshList();
    }
}
