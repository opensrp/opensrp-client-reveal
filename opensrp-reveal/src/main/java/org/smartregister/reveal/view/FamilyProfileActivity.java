package org.smartregister.reveal.view;

import android.content.Intent;
import android.support.v4.view.ViewPager;

import org.smartregister.family.activity.BaseFamilyProfileActivity;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.family.util.Constants;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.fragment.FamilyProfileMemberFragment;
import org.smartregister.reveal.fragment.StructureTasksFragment;
import org.smartregister.reveal.model.FamilyProfileModel;
import org.smartregister.reveal.presenter.FamilyProfilePresenter;

import static org.smartregister.reveal.util.Constants.REQUEST_CODE_GET_JSON_FRAGMENT;

/**
 * Created by samuelgithengi on 2/8/19.
 */
public class FamilyProfileActivity extends BaseFamilyProfileActivity implements FamilyProfileContract.View {

    private StructureTasksFragment structureTasksFragment;


    @Override
    protected void initializePresenter() {
        String familyBaseEntityId = getIntent().getStringExtra(Constants.INTENT_KEY.FAMILY_BASE_ENTITY_ID);
        String familyHead = getIntent().getStringExtra(Constants.INTENT_KEY.FAMILY_HEAD);
        String primaryCaregiver = getIntent().getStringExtra(Constants.INTENT_KEY.PRIMARY_CAREGIVER);
        String familyName = getIntent().getStringExtra(Constants.INTENT_KEY.FAMILY_NAME);

        presenter = new FamilyProfilePresenter(this,
                new FamilyProfileModel(familyName), familyBaseEntityId, familyHead, primaryCaregiver, familyName);
    }

    @Override
    protected ViewPager setupViewPager(ViewPager viewPager) {
        adapter = new ViewPagerAdapter(getSupportFragmentManager());

        FamilyProfileMemberFragment profileMemberFragment = FamilyProfileMemberFragment.newInstance(this.getIntent().getExtras());
        adapter.addFragment(profileMemberFragment, this.getString(R.string.residents).toUpperCase());

        structureTasksFragment = StructureTasksFragment.newInstance(this.getIntent().getExtras());
        adapter.addFragment(structureTasksFragment, this.getString(R.string.tasks).toUpperCase());

        viewPager.setAdapter(adapter);

        return viewPager;
    }

    @Override
    public void setProfileImage(String baseEntityId) {
        //do nothing
    }

    @Override
    public void setStructureId(String structureId) {
        structureTasksFragment.setStructure(structureId);
    }

    @Override
    public void refreshTasks(String structureId) {
        structureTasksFragment.refreshTasks(structureId);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == io.ona.kujaku.utils.Constants.RequestCode.LOCATION_SETTINGS ||
                requestCode == REQUEST_CODE_GET_JSON_FRAGMENT) {
            structureTasksFragment.onActivityResult(requestCode, resultCode, data);
        } else {
            super.onActivityResult(requestCode, resultCode, data);
        }
    }
}
