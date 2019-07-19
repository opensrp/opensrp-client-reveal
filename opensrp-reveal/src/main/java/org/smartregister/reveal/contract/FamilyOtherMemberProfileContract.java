package org.smartregister.reveal.contract;

import android.content.Context;

import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.family.contract.FamilyOtherMemberContract;

/**
 * Created by samuelgithengi on 5/31/19.
 */
public interface FamilyOtherMemberProfileContract {

    interface BasePresenter {
        void onFetchFamilyHead(CommonPersonObject commonPersonObject);
    }

    interface Presenter extends BasePresenter, FamilyOtherMemberContract.Presenter {
        void onEditMemberDetails();

        void updateFamilyMember(String jsonString);
    }

    interface View extends FamilyOtherMemberContract.View {

        void startFormActivity(JSONObject jsonForm);

        Context getContext();

        void refreshList();
    }


    interface Interactor extends FamilyOtherMemberContract.Interactor {

        void getFamilyHead(BasePresenter presenter, String familyHeadId);
    }
}
