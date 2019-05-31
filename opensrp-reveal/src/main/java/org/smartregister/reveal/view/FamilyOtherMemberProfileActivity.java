package org.smartregister.reveal.view;

import android.content.Context;
import android.content.Intent;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;

import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.domain.Form;

import org.json.JSONObject;
import org.smartregister.family.activity.BaseFamilyOtherMemberProfileActivity;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.family.model.BaseFamilyOtherMemberProfileActivityModel;
import org.smartregister.family.util.Constants;
import org.smartregister.family.util.JsonFormUtils;
import org.smartregister.family.util.Utils;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.fragment.FamilyOtherMemberProfileFragment;
import org.smartregister.reveal.presenter.FamilyOtherMemberPresenter;

public class FamilyOtherMemberProfileActivity extends BaseFamilyOtherMemberProfileActivity implements FamilyOtherMemberProfileContract.View {

    @Override
    protected void initializePresenter() {
        String baseEntityId = getIntent().getStringExtra(Constants.INTENT_KEY.BASE_ENTITY_ID);
        String familyHead = getIntent().getStringExtra(Constants.INTENT_KEY.FAMILY_HEAD);
        String primaryCaregiver = getIntent().getStringExtra(Constants.INTENT_KEY.PRIMARY_CAREGIVER);
        String villageTown = getIntent().getStringExtra(Constants.INTENT_KEY.VILLAGE_TOWN);
        String familyName = getIntent().getStringExtra(Constants.INTENT_KEY.FAMILY_NAME);
        presenter = new FamilyOtherMemberPresenter(this, new BaseFamilyOtherMemberProfileActivityModel(),
                null, baseEntityId, familyHead, primaryCaregiver, villageTown, familyName);
    }

    @Override
    protected ViewPager setupViewPager(ViewPager viewPager) {
        adapter = new ViewPagerAdapter(getSupportFragmentManager());
        FamilyOtherMemberProfileFragment profileOtherMemberFragment = FamilyOtherMemberProfileFragment.newInstance(this.getIntent().getExtras());
        adapter.addFragment(profileOtherMemberFragment, "");

        viewPager.setAdapter(adapter);

        return viewPager;
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);

        MenuItem addMember = menu.findItem(R.id.add_member);
        if (addMember != null) {
            addMember.setVisible(false);
        }

        getMenuInflater().inflate(R.menu.other_member_menu, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                onBackPressed();
                return true;
            case R.id.action_registration:
                presenter().onEditMemberDetails();
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    public void startFormActivity(JSONObject jsonForm) {

        Intent intent = new Intent(this, Utils.metadata().familyMemberFormActivity);
        intent.putExtra(Constants.JSON_FORM_EXTRA.JSON, jsonForm.toString());

        Form form = new Form();
        form.setActionBarBackground(R.color.family_actionbar);
        form.setWizard(false);
        intent.putExtra(JsonFormConstants.JSON_FORM_KEY.FORM, form);

        startActivityForResult(intent, JsonFormUtils.REQUEST_CODE_GET_JSON);
    }

    @Override
    public Context getContext() {
        return this;
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == JsonFormUtils.REQUEST_CODE_GET_JSON && resultCode == RESULT_OK) {
            try {
                String jsonString = data.getStringExtra(Constants.JSON_FORM_EXTRA.JSON);
                JSONObject form = new JSONObject(jsonString);
                if (form.getString(JsonFormUtils.ENCOUNTER_TYPE).equals(Utils.metadata().familyMemberRegister.updateEventType)) {
                    presenter().updateFamilyMember(jsonString);
                }
            } catch (Exception e) {
                Log.e(TAG, "Error processing form submission", e);
            }

        }
    }

    @Override
    public FamilyOtherMemberProfileContract.Presenter presenter() {
        return (FamilyOtherMemberProfileContract.Presenter) super.presenter();
    }
}
