package org.smartregister.reveal.view;

import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.NavUtils;
import android.view.MenuItem;

import org.smartregister.family.activity.BaseFamilyRegisterActivity;
import org.smartregister.family.util.Utils;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FamilyRegisterContract;
import org.smartregister.reveal.fragment.FamilyRegisterFragment;
import org.smartregister.reveal.model.FamilyRegisterModel;
import org.smartregister.reveal.presenter.FamilyRegisterPresenter;
import org.smartregister.reveal.util.Constants.Properties;
import org.smartregister.view.fragment.BaseRegisterFragment;

/**
 * Created by samuelgithengi on 2/8/19.
 */
public class FamilyRegisterActivity extends BaseFamilyRegisterActivity implements FamilyRegisterContract.View {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getIntent().getBooleanExtra("START_REGISTRATION", false)) {
            startRegistration();
        }
    }

    @Override
    protected void initializePresenter() {
        String structureId = getIntent().getStringExtra(Properties.LOCATION_UUID);
        String taskId = getIntent().getStringExtra(Properties.TASK_IDENTIFIER);
        String taskBusinessStatus = getIntent().getStringExtra(Properties.TASK_BUSINESS_STATUS);
        String taskStatus = getIntent().getStringExtra(Properties.TASK_STATUS);
        String structureName = getIntent().getStringExtra(Properties.STRUCTURE_NAME);
        presenter = new FamilyRegisterPresenter(this, new FamilyRegisterModel(structureId, taskId, taskBusinessStatus, taskStatus, structureName));
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
        bottomNavigationView.getMenu().removeItem(R.id.action_register);
    }

    @Override
    public void switchToFragment(int position) {
        if (position == 0) {
            NavUtils.navigateUpFromSameTask(this);
            return;
        }
        super.switchToFragment(position);
    }

    @Override
    public void startProfileActivity(String baseEntityId, String familyHead, String primaryCareGiver, String firstName) {
        Intent intent = new Intent(this, Utils.metadata().profileActivity);
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.FAMILY_BASE_ENTITY_ID, baseEntityId);
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.FAMILY_HEAD, familyHead);
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.PRIMARY_CAREGIVER, primaryCareGiver);
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.FAMILY_NAME, firstName);
        intent.putExtra(org.smartregister.family.util.Constants.INTENT_KEY.GO_TO_DUE_PAGE, false);

        startActivity(intent);
        finish();
    }
}
