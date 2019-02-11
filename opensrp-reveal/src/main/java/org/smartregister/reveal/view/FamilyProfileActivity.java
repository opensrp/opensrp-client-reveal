package org.smartregister.reveal.view;

import android.support.v4.view.ViewPager;

import org.smartregister.family.activity.BaseFamilyProfileActivity;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.family.fragment.BaseFamilyProfileMemberFragment;
import org.smartregister.family.model.BaseFamilyProfileModel;
import org.smartregister.family.presenter.BaseFamilyProfilePresenter;
import org.smartregister.family.util.Constants;
import org.smartregister.reveal.fragment.FamilyProfileMemberFragment;

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
        presenter = new BaseFamilyProfilePresenter(this, new BaseFamilyProfileModel(familyName), familyBaseEntityId, familyHead, primaryCaregiver, familyName);
    }

    @Override
    protected ViewPager setupViewPager(ViewPager viewPager) {
        adapter = new ViewPagerAdapter(getSupportFragmentManager());

        BaseFamilyProfileMemberFragment profileMemberFragment = FamilyProfileMemberFragment.newInstance(this.getIntent().getExtras());
        adapter.addFragment(profileMemberFragment, this.getString(org.smartregister.family.R.string.member).toUpperCase());

        viewPager.setAdapter(adapter);

        return viewPager;
    }

}
