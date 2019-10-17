package org.smartregister.reveal.contract;

import org.json.JSONObject;

public interface OtherFormsContract {

    interface View {

        void startFormActivity(JSONObject jsonObject);

        void saveJsonForm(String json);

    }

    interface Presenter extends BaseContract.BasePresenter {

        void startFormActivity(JSONObject jsonObject);

        void saveJsonForm(String json);

    }

    interface Interactor extends BaseContract.BaseInteractor {

    }
}
