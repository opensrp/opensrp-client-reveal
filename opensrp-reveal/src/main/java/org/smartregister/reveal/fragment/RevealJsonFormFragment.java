package org.smartregister.reveal.fragment;

import android.os.Bundle;

import androidx.annotation.Nullable;

import android.view.Menu;
import android.view.MenuItem;
import android.view.View;

import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.presenters.JsonFormFragmentPresenter;

import org.smartregister.reveal.R;
import org.smartregister.reveal.interactor.RevealJsonFormInteractor;
import org.smartregister.reveal.presenter.RevealJsonFormFragmentPresenter;
import org.smartregister.reveal.util.Constants;

/**
 * Created by samuelgithengi on 12/13/18.
 */
public class RevealJsonFormFragment extends JsonFormFragment {

    private RevealJsonFormFragmentPresenter presenter;

    private boolean readOnly;

    public RevealJsonFormFragment(){
        this(false);
    }

    public RevealJsonFormFragment(boolean readOnly){
        this.readOnly = readOnly;
    }

    @Override
    protected JsonFormFragmentPresenter createPresenter() {
        presenter = new RevealJsonFormFragmentPresenter(this, new RevealJsonFormInteractor());
        return presenter;
    }

    public static RevealJsonFormFragment getFormFragment(String stepName, boolean readOnly) {
        RevealJsonFormFragment jsonFormFragment = new RevealJsonFormFragment(readOnly);
        Bundle bundle = new Bundle();
        bundle.putString(JsonFormConstants.JSON_FORM_KEY.STEPNAME, stepName);
        jsonFormFragment.setArguments(bundle);
        return jsonFormFragment;
    }

    @Override
    public void onViewCreated(View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        setupMargins(view);
    }

    private void setupMargins(View view) {
        if (getArguments() != null) {
            String stepName = getArguments().getString(JsonFormConstants.STEPNAME);
            if (getStep(stepName).optBoolean(Constants.JsonForm.NO_PADDING)) {
                view.findViewById(R.id.main_layout).setPadding(0, 0, 0, 0);
            }
        }
    }

    @Override
    public boolean save(boolean skipValidation) {
        // Heads up
        return super.save(skipValidation);
    }

    @Override
    public void updateVisibilityOfNextAndSave(boolean next, boolean save) {
        // Heads up
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

        super.updateVisibilityOfNextAndSave(next, save);
    }


    public RevealJsonFormFragmentPresenter getPresenter() {
        return presenter;
    }

}
