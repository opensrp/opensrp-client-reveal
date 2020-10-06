package org.smartregister.reveal.presenter;

import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonWizardFormFragment;
import com.vijay.jsonwizard.interactors.JsonFormInteractor;
import com.vijay.jsonwizard.presenters.JsonWizardFormFragmentPresenter;
import com.vijay.jsonwizard.views.JsonFormFragmentView;

import org.smartregister.reveal.fragment.ReadableJsonWizardFormFragment;

public class ReadableJsonWizardFormFragmentPresenter extends JsonWizardFormFragmentPresenter {
    private boolean readOnly;

    public ReadableJsonWizardFormFragmentPresenter(ReadableJsonWizardFormFragment formFragment, JsonFormInteractor jsonFormInteractor) {
        super(formFragment, jsonFormInteractor);

        this.readOnly = formFragment.isReadOnly();
    }

    @Override
    protected boolean moveToNextWizardStep() {
        if(!this.readOnly){
            return super.moveToNextWizardStep();
        }
        else {
            if ("".equals(this.mStepDetails.optString(JsonFormConstants.NEXT))) {
                return false;
            } else {
                JsonWizardFormFragment next = ReadableJsonWizardFormFragment.getFormFragment(this.mStepDetails.optString(JsonFormConstants.NEXT), this.readOnly);
                ((JsonFormFragmentView)this.getView()).hideKeyBoard();
                ((JsonFormFragmentView)this.getView()).transactThis(next);

                return true;
            }
        }
    }
}
