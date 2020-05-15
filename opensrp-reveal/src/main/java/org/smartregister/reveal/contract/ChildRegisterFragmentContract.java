package org.smartregister.reveal.contract;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.annotation.WorkerThread;

import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.domain.Task;
import org.smartregister.reveal.model.Child;
import org.smartregister.util.CallableInteractor;
import org.smartregister.view.ListContract;

import java.util.HashMap;
import java.util.List;

public interface ChildRegisterFragmentContract {

    interface View extends ListContract.View<Child> {
        void openFilterFragment();

        void startChildRegistrationForm();

        void startRecordMDAForm(String baseEntityID);

        void startJsonForm(JSONObject form, String formTitle);

        void reloadFromSource();
    }

    interface Presenter extends ListContract.Presenter<Child> {

        void search(@Nullable HashMap<String, List<String>> sortAndFilter, @Nullable String searchText);

        void startMDAForm(Context context, String baseEntityID);

        void startChildRegistrationForm(Context context);

        @NonNull
        CallableInteractor getCallableInteractor();

        void saveChild(String jsonString, Context context);

        void saveMDAForm(String jsonString, Context context);
    }

    interface Model extends ListContract.Model<Child> {

        @WorkerThread
        List<Child> searchAndFilter(@Nullable HashMap<String, List<String>> sortAndFilter, @Nullable String searchText);

        @WorkerThread
        JSONObject getMDAForm(Context context, String baseEntityID) throws JSONException;

        @WorkerThread
        JSONObject getRegistrationForm(Context context) throws JSONException;

        Task getCurrentTask(Context context, String baseEntityID);

        String getCurrentLocationID();
    }

}
