package org.smartregister.reveal.contract;

import org.json.JSONObject;

public interface OtherFormsContract {

    interface View {

        void startFormActivity(JSONObject jsonObject);

        void saveJsonForm(String json);

        void showProgressDialog(int titleIdentifier);

        void hideProgressDialog();

    }

    interface Presenter extends BaseContract.BasePresenter {

        void saveJsonForm(String json);

    }

    interface Interactor extends BaseContract.BaseInteractor {

    }
}
