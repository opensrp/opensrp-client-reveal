package org.smartregister.reveal.view;

import android.support.v4.view.ViewPager;

import org.smartregister.family.activity.BaseFamilyProfileActivity;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.family.util.Constants;
import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.FamilyProfileMemberFragment;
import org.smartregister.reveal.fragment.StructureTasksFragment;
import org.smartregister.reveal.model.FamilyProfileModel;
import org.smartregister.reveal.presenter.FamilyProfilePresenter;
import org.smartregister.reveal.util.Constants.Properties;

/**
 * Created by samuelgithengi on 2/8/19.
 */
public class FamilyProfileActivity extends BaseFamilyProfileActivity {

    @Override
    protected void initializePresenter() {
        String familyBaseEntityId = getIntent().getStringExtra(Constants.INTENT_KEY.FAMILY_BASE_ENTITY_ID);
        String familyHead = getIntent().getStringExtra(Constants.INTENT_KEY.FAMILY_HEAD);
        String primaryCaregiver = getIntent().getStringExtra(Constants.INTENT_KEY.PRIMARY_CAREGIVER);
        String familyName = getIntent().getStringExtra(Constants.INTENT_KEY.FAMILY_NAME);
        presenter = new FamilyProfilePresenter(this,
                new FamilyProfileModel(familyName, familyBaseEntityId), familyBaseEntityId, familyHead, primaryCaregiver, familyName);
    }

    @Override
    protected ViewPager setupViewPager(ViewPager viewPager) {
        adapter = new ViewPagerAdapter(getSupportFragmentManager());

        FamilyProfileMemberFragment profileMemberFragment = FamilyProfileMemberFragment.newInstance(this.getIntent().getExtras());
        adapter.addFragment(profileMemberFragment, this.getString(R.string.residents).toUpperCase());


        StructureTasksFragment structureTasksFragment = StructureTasksFragment.newInstance(this.getIntent().getExtras());
        adapter.addFragment(structureTasksFragment, this.getString(R.string.tasks).toUpperCase());

        viewPager.setAdapter(adapter);

        return viewPager;
    }

    @Override
    public void setProfileImage(String baseEntityId) {
        //do nothing
    }
}
