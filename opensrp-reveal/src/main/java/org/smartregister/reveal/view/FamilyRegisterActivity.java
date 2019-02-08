package org.smartregister.reveal.view;

import android.support.v4.app.Fragment;

import org.smartregister.family.activity.BaseFamilyRegisterActivity;
import org.smartregister.reveal.fragment.FamilyRegisterFragment;
import org.smartregister.reveal.model.FamilyRegisterModel;
import org.smartregister.reveal.presenter.FamilyRegisterPresenter;
import org.smartregister.view.fragment.BaseRegisterFragment;

/**
 * Created by samuelgithengi on 2/8/19.
 */
public class FamilyRegisterActivity extends BaseFamilyRegisterActivity {

    @Override
    protected void initializePresenter() {
        presenter = new FamilyRegisterPresenter(this, new FamilyRegisterModel());
    }

    @Override
    protected BaseRegisterFragment getRegisterFragment() {
        return new FamilyRegisterFragment();
    }

    @Override
    protected Fragment[] getOtherFragments() {
        return new Fragment[0];
    }


}
