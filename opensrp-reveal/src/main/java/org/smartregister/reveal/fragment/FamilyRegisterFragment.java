package org.smartregister.reveal.fragment;

import org.smartregister.family.fragment.BaseFamilyRegisterFragment;
import org.smartregister.family.model.BaseFamilyRegisterFramentModel;
import org.smartregister.reveal.presenter.FamilyRegisterFragmentPresenter;
import org.smartregister.view.activity.BaseRegisterActivity;

/**
 * Created by samuelgithengi on 2/8/19.
 */
public class FamilyRegisterFragment extends BaseFamilyRegisterFragment {
    @Override
    protected void initializePresenter() {
        if (getActivity() == null) {
            return;
        }

        String viewConfigurationIdentifier = ((BaseRegisterActivity) getActivity()).getViewIdentifiers().get(0);
        presenter = new FamilyRegisterFragmentPresenter(this, new BaseFamilyRegisterFramentModel(), viewConfigurationIdentifier);
    }

    @Override
    protected String getMainCondition() {
        return presenter().getMainCondition();
    }

    @Override
    protected String getDefaultSortQuery() {
        return presenter().getDefaultSortQuery();
    }


}
