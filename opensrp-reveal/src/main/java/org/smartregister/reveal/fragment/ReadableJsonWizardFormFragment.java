package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;

import androidx.annotation.Nullable;

import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonWizardFormFragment;
import com.vijay.jsonwizard.interactors.JsonFormInteractor;
import com.vijay.jsonwizard.presenters.JsonFormFragmentPresenter;

import org.smartregister.reveal.R;
import org.smartregister.reveal.presenter.ReadableJsonWizardFormFragmentPresenter;

public class ReadableJsonWizardFormFragment extends JsonWizardFormFragment {
    private boolean readOnly;

    public ReadableJsonWizardFormFragment(){
        this(false);
    }

    public ReadableJsonWizardFormFragment(boolean readOnly){
        this.readOnly = readOnly;
    }

    public boolean isReadOnly() {
        return readOnly;
    }

    @Override
    protected JsonFormFragmentPresenter createPresenter() {
        presenter = new ReadableJsonWizardFormFragmentPresenter(this, JsonFormInteractor.getInstance());
        return presenter;
    }

    @Override
    public void onViewCreated(View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        this.hideKeyBoard();
    }


    @Override
    public void updateVisibilityOfNextAndSave(boolean next, boolean save) {
        super.updateVisibilityOfNextAndSave(next, save);

        // HEADS UP
        if(this.readOnly) {
            Menu menu = this.getMenu();
            MenuItem item = menu.findItem(R.id.action_save);
            item.setTitle("Done");
            item.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
                @Override
                public boolean onMenuItemClick(MenuItem item) {
                    getActivity().finish();

                    return true;
                }
            });
        }
    }


    public static ReadableJsonWizardFormFragment getFormFragment(String stepName, boolean readOnly) {
        ReadableJsonWizardFormFragment jsonFormFragment = new ReadableJsonWizardFormFragment(readOnly);
        Bundle bundle = new Bundle();
        bundle.putString(JsonFormConstants.JSON_FORM_KEY.STEPNAME, stepName);
        jsonFormFragment.setArguments(bundle);
        return jsonFormFragment;
    }
}
