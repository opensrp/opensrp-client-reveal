package org.smartregister.reveal.view;

import android.support.v4.app.Fragment;
import android.support.v4.app.NavUtils;
import android.view.MenuItem;

import org.smartregister.family.activity.BaseFamilyRegisterActivity;
import org.smartregister.family.model.BaseFamilyRegisterModel;
import org.smartregister.family.presenter.BaseFamilyRegisterPresenter;
import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.FamilyRegisterFragment;
import org.smartregister.view.fragment.BaseRegisterFragment;

/**
 * Created by samuelgithengi on 2/8/19.
 */
public class FamilyRegisterActivity extends BaseFamilyRegisterActivity {

    @Override
    protected void initializePresenter() {
        presenter = new BaseFamilyRegisterPresenter(this, new BaseFamilyRegisterModel());
    }

    @Override
    protected BaseRegisterFragment getRegisterFragment() {
        return new FamilyRegisterFragment();
    }

    @Override
    protected Fragment[] getOtherFragments() {
        return new Fragment[0];
    }

    @Override
    protected void registerBottomNavigation() {
        super.registerBottomNavigation();

        MenuItem clients = bottomNavigationView.getMenu().findItem(R.id.action_clients);
        if (clients != null) {
            clients.setTitle(getString(R.string.families));
        }

        bottomNavigationView.getMenu().removeItem(R.id.action_search);
        bottomNavigationView.getMenu().removeItem(R.id.action_library);
        bottomNavigationView.getMenu().removeItem(R.id.action_job_aids);
    }

    @Override
    public void switchToFragment(int position) {
        if (position == 0) {
            NavUtils.navigateUpFromSameTask(this);
            return;
        }
        super.switchToFragment(position);
    }
}
